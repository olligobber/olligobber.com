{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module Main where

import Data.Either (partitionEithers)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.String (fromString)
import qualified Data.Text.Lazy.IO as TIO
import Data.Time.Calendar.OrdinalDate (Day)
import Data.Time.Format (formatTime, defaultTimeLocale, parseTimeM)
import System.Directory (copyFile, createDirectoryIfMissing, removeFile)
import System.Process (callProcess)
import qualified Text.Atom.Feed as F
import Text.Feed.Export (textFeed)
import Text.Feed.Types (Feed(AtomFeed))

data Post = Post
	{ title :: String
	, description :: String
	, date :: Day
	, path :: String
	, tags :: [String]
	}

ordinal :: String -> String
ordinal s
	| last (take 2 $ reverse s) == '1' = s <> "th"
	| last s == '1' = s <> "st"
	| last s == '2' = s <> "nd"
	| last s == '3' = s <> "rd"
	| otherwise = s <> "th"

showDate :: Day -> String
showDate d =
	ordinal (formatTime defaultTimeLocale "%e" d) <>
	formatTime defaultTimeLocale " %B %Y" d

parsePost :: String -> Either String Post
parsePost input = do
	(titleLine, descriptionLine, dateLine, pathLine, tagsLine) <-
		case lines input of
			[a,b,c,d,e] -> Right (a,b,c,d,e)
			x | length x < 5 -> Left "Not enough lines in post section"
			_ -> Left "Too many lines in post section"
	title <- case span (/= ' ') titleLine of
		("title:", x) -> Right $ tail x
		_ -> Left "First line in post should be title"
	description <- case span (/= ' ') descriptionLine of
		("description:", x) -> Right $ tail x
		_ -> Left "Second line in post should be description"
	date <- case span (/= ' ') dateLine of
		("date:", x) -> case parseTimeM True defaultTimeLocale "%Y/%m/%d" x of
			Nothing -> Left "Failed to parse date"
			Just x -> Right x
		_ -> Left "Third line in post should be date"
	path <- case span (/= ' ') pathLine of
		("path:", x) -> Right $ tail x
		_ -> Left "Fourth line in post should be path"
	tags <- case span (/= ' ') tagsLine of
		("tags:", x) -> Right $ words x
		_ -> Left "Fifth line in post should be tags"
	pure $ Post { title, description, path, date, tags }

parsePosts :: String -> Either String [Post]
parsePosts input = case partitionEithers parsedPosts of
	([], x) -> Right x
	(e, _) -> Left $ unlines e
	where
		parsedPosts = parsePost <$> splitOn "\n\n" input

data Templates = Templates
	{ allPosts :: String
	, allTags :: String
	, homePage :: String
	, postDiv :: String
	, postPage :: String
	, tagLink :: String
	, taggedPosts :: String
	, tagBig :: String
	}

loadTemplates :: IO Templates
loadTemplates = do
	allPosts <- readFile "templates/all_posts.html"
	allTags <- readFile "templates/all_tags.html"
	homePage <- readFile "templates/home_page.html"
	postDiv <- readFile "templates/post_div.html"
	postPage <- readFile "templates/post.html"
	tagLink <- readFile "templates/tag.html"
	taggedPosts <- readFile "templates/tagged_posts.html"
	tagBig <- readFile "templates/tag_big.html"
	pure $ Templates
		{ allPosts
		, allTags
		, homePage
		, postDiv
		, postPage
		, tagLink
		, taggedPosts
		, tagBig
		}

substitute :: Eq a => [a] -> [a] -> [a] -> [a]
substitute pattern replacement template =
	intercalate replacement $ splitOn pattern template

makeTagLink :: Templates -> String -> String
makeTagLink templates tag =
	substitute "{url}" ("/tag/" <> tag) $
	substitute "{name}" tag $
	tagLink templates

makePostDiv :: Templates -> Post -> String
makePostDiv templates post =
	substitute "{url}" ("/" <> path post) $
	substitute "{name}" (title post) $
	substitute "{date}" (showDate $ date post) $
	substitute "{description}" (description post) $
	substitute "{tags}" allTags $
	postDiv templates
	where
		allTags = foldMap (makeTagLink templates) $ tags post

makeHomePage :: Templates -> [Post] -> String
makeHomePage templates posts =
	substitute "{posts}" recentPosts $ homePage templates where
		recentPosts = foldMap (makePostDiv templates) $ take 3 posts

makeAllPosts :: Templates -> [Post] -> String
makeAllPosts templates posts =
	substitute "{posts}" postsList $ allPosts templates where
		postsList = foldMap (makePostDiv templates) posts

collectTags :: [Post] -> Map String [Post]
collectTags = M.fromListWith (flip (<>)) . concatMap tagPost where
	tagPost post = (\x -> (x, [post])) <$> tags post

makeBigTag :: Templates -> String -> String
makeBigTag templates tag =
	substitute "{url}" ("/tag/" <> tag) $
	substitute "{name}" tag $
	tagBig templates

makeAllTags :: Templates -> [String] -> String
makeAllTags templates tags =
	substitute "{tags}" (foldMap (makeBigTag templates) tags) $
	allTags templates

makeTaggedPosts :: Templates -> String -> [Post] -> String
makeTaggedPosts templates tag posts =
	substitute "{tag}" tag $
	substitute "{posts}" renderedPosts $
	taggedPosts templates
	where
		renderedPosts = foldMap (makePostDiv templates) posts

writeTaggedPosts :: Templates -> Map String [Post] -> IO ()
writeTaggedPosts templates tagmap = M.foldMapWithKey writeOne tagmap where
	writeOne tag posts = do
		createDirectoryIfMissing True $ "docs/tag/" <> tag
		writeFile ("docs/tag/" <> tag <> "/index.html") $
			makeTaggedPosts templates tag posts

extractBody :: String -> String
extractBody = head . splitOn "</body>" . head . tail . splitOn "<body>"

compilePost :: Templates -> Post -> IO ()
compilePost templates post = do
	callProcess "typst"
		[ "c"
		, "--features"
		, "html"
		, "--format"
		, "html"
		, "posts/" <> path post <> "/main.typ"
		]
	html <- extractBody <$> readFile ("posts/" <> path post <> "/main.html")
	createDirectoryIfMissing True $ "docs/" <> path post
	writeFile ("docs/" <> path post <> "/index.html") $
		substitute "{title}" (title post) $
		substitute "{date}" (showDate $ date post) $
		substitute "{tags}" (foldMap (makeTagLink templates) $ tags post) $
		substitute "{path}" (path post) $
		substitute "{description}" (description post) $
		substitute "{content}" html $
		postPage templates
	removeFile ("posts/" <> path post <> "/main.html")

feedEntry :: Post -> F.Entry
feedEntry post = F.Entry
	{ F.entryId = fromString $ "https://olligobber.com/" <> path post
	, F.entryTitle = F.TextString $ fromString $ title post
	, F.entryUpdated = fromString $ formatTime defaultTimeLocale "%Y-%m-%d" $ date post
	, F.entryAuthors = []
	, F.entryCategories = []
	, F.entryContent = Nothing
	, F.entryContributor = []
	, F.entryLinks = [F.nullLink $ fromString $ "https://olligobber.com/" <> path post]
	, F.entryPublished = Nothing
	, F.entryRights = Nothing
	, F.entrySource = Nothing
	, F.entrySummary = Just $ F.TextString $ fromString $ description post
	, F.entryInReplyTo = Nothing
	, F.entryInReplyTotal = Nothing
	, F.entryAttrs = []
	, F.entryOther = []
	}

feed :: [Post] -> F.Feed
feed posts = F.Feed
	{ F.feedId = "https://olligobber.com/atom.xml"
	, F.feedTitle = F.TextString "olligobber.com"
	, F.feedUpdated = fromString $ formatTime defaultTimeLocale "%Y-%m-%d" $ date $ head posts
	, F.feedAuthors = []
	, F.feedCategories = []
	, F.feedContributors = []
	, F.feedGenerator = Nothing
	, F.feedIcon = Just "https://olligobber.com/Logo.png"
	, F.feedLinks = [F.nullLink "https://olligobber.com"]
	, F.feedLogo = Just "https://olligobber.com/Logo.png"
	, F.feedRights = Nothing
	, F.feedSubtitle = Nothing
	, F.feedEntries = feedEntry <$> posts
	, F.feedAttrs = []
	, F.feedOther = []
	}

main :: IO ()
main = do
	putStrLn "Starting setup of website"
	parsedPosts <- parsePosts <$> readFile "posts/list"
	posts <- case parsedPosts of
		Left e -> error $ "Error reading list of posts in posts/list:\n" <> e
		Right x -> pure x
	templates <- loadTemplates
	writeFile "docs/index.html" $ makeHomePage templates posts
	createDirectoryIfMissing False "docs/posts"
	writeFile "docs/posts/index.html" $ makeAllPosts templates posts
	let tagmap = collectTags posts
	createDirectoryIfMissing False "docs/tags"
	writeFile "docs/tags/index.html" $ makeAllTags templates $ M.keys tagmap
	writeTaggedPosts templates tagmap
	foldMap (compilePost templates) posts
	TIO.writeFile "docs/atom.xml" $ fromJust $ textFeed $ AtomFeed $ feed posts
	putStrLn "Done!"