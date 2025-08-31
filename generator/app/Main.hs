{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Either (partitionEithers)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import System.Directory (copyFile, createDirectoryIfMissing)

data Post = Post
	{ title :: String
	, description :: String
	, date :: String
	, path :: String
	, tags :: [String]
	}

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
		("date:", x) -> Right $ tail x
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
	pure $ Templates {allPosts, allTags, homePage, postDiv, postPage, tagLink, taggedPosts}

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
	substitute "{date}" (date post) $
	substitute "{description}" (description post) $
	substitute "{tags}" allTags $
	postDiv templates
	where
		allTags = foldMap (makeTagLink templates) $ tags post

makeHomePage :: Templates -> [Post] -> String
makeHomePage templates posts =
	substitute "{posts}" recentPosts $ homePage templates where
		recentPosts = foldMap (makePostDiv templates) $ take 3 posts

-- Copy all the static files to the right places
copyStatic :: IO ()
copyStatic = do
	createDirectoryIfMissing False "docs"
	copyFile "static/404.html" "docs/404.html"
	copyFile "static/CNAME" "docs/CNAME"
	copyFile "static/Logo.png" "docs/Logo.png"
	copyFile "static/style.css" "docs/style.css"
	createDirectoryIfMissing False "docs/about"
	copyFile "static/about_me.html" "docs/about/index.html"

main :: IO ()
main = do
	parsedPosts <- parsePosts <$> readFile "posts/list"
	posts <- case parsedPosts of
		Left e -> error $ "Error reading lift of posts in posts/list:\n" <> e
		Right x -> pure x
	templates <- loadTemplates
	copyStatic
	writeFile "docs/index.html" $ makeHomePage templates posts