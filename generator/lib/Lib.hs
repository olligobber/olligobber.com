{-# LANGUAGE NamedFieldPuns, TupleSections #-}

module Lib
	( Post(..)
	, showDate
	, loadPosts
	, substitute
	, PostDivTemplates
	, loadPostDivTemplates
	, makeTagLink
	, makePostDiv
	, collectTags
	)
where

import Data.Either (partitionEithers)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Time.Calendar.OrdinalDate (Day)
import Data.Time.Format (formatTime, defaultTimeLocale, parseTimeM)

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

loadPosts :: IO [Post]
loadPosts = do
	parsedPosts <- parsePosts <$> readFile "posts/list"
	case parsedPosts of
		Left e -> error $ "Error reading list of posts in posts/list:\n" <> e
		Right x -> pure x

substitute :: Eq a => [a] -> [a] -> [a] -> [a]
substitute before after template =
	intercalate after $ splitOn before template

data PostDivTemplates = PostDivTemplates
	{ postDiv :: String
	, tagLink :: String
	}

loadPostDivTemplates :: IO PostDivTemplates
loadPostDivTemplates = do
	postDiv <- readFile "templates/post_div.html"
	tagLink <- readFile "templates/tag.html"
	pure $ PostDivTemplates
		{ postDiv
		, tagLink
		}

makeTagLink :: PostDivTemplates -> String -> String
makeTagLink templates tag =
	substitute "{url}" ("/tag/" <> tag) $
	substitute "{name}" tag $
	tagLink templates

makePostDiv :: PostDivTemplates -> Post -> String
makePostDiv templates post =
	substitute "{url}" ("/" <> path post) $
	substitute "{name}" (title post) $
	substitute "{date}" (showDate $ date post) $
	substitute "{description}" (description post) $
	substitute "{tags}" allTags $
	postDiv templates
	where
		allTags = foldMap (makeTagLink templates) $ tags post

collectTags :: [Post] -> Map String [Post]
collectTags = M.fromListWith (flip (<>)) . concatMap tagPost where
	tagPost post = (, [post]) <$> tags post