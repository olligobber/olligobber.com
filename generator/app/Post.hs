{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Foldable (for_)
import Data.List.Split (splitOn)
import System.Environment (getArgs)

import Lib
	( Post(..), PostDivTemplates
	, loadPostDivTemplates, makeTagLink, substitute, loadPosts, showDate
	)

data Templates = Templates
	{ postPage :: String
	, postDiv :: PostDivTemplates
	}

loadTemplates :: IO Templates
loadTemplates = do
	postPage <- readFile "templates/post.html"
	postDiv <- loadPostDivTemplates
	pure $ Templates {postPage, postDiv}

extractBody :: String -> String
extractBody = head . splitOn "</body>" . head . tail . splitOn "<body>"

compilePost :: Templates -> Post -> IO ()
compilePost templates post = do
	html <- extractBody <$> readFile ("typst_build/" <> path post <> ".html")
	writeFile ("docs/" <> path post <> "/index.html") $
		substitute "{title}" (title post) $
		substitute "{date}" (showDate $ date post) $
		substitute "{tags}" (foldMap (makeTagLink $ postDiv templates) $ tags post) $
		substitute "{path}" (path post) $
		substitute "{description}" (description post) $
		substitute "{content}" html $
		postPage templates

main :: IO ()
main = do
	paths <- getArgs
	posts <- loadPosts
	templates <- loadTemplates
	for_ paths $ \thispath ->
		compilePost templates $
		head $
		filter (\post -> path post == thispath) posts