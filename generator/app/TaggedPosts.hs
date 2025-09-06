{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Foldable (for_)
import qualified Data.Map as M
import System.Environment (getArgs)

import Lib
	( Post, PostDivTemplates
	, loadPostDivTemplates, makePostDiv, substitute, loadPosts, collectTags
	)

data Templates = Templates
	{ taggedPosts :: String
	, postDiv :: PostDivTemplates
	}

loadTemplates :: IO Templates
loadTemplates = do
	taggedPosts <- readFile "templates/tagged_posts.html"
	postDiv <- loadPostDivTemplates
	pure $ Templates {taggedPosts, postDiv}

makeTaggedPosts :: Templates -> String -> [Post] -> String
makeTaggedPosts templates tag posts =
	substitute "{tag}" tag $
	substitute "{posts}" renderedPosts $
	taggedPosts templates
	where
		renderedPosts = foldMap (makePostDiv $ postDiv templates) posts

main :: IO ()
main = do
	tags <- getArgs
	tagMap <- collectTags <$> loadPosts
	templates <- loadTemplates
	for_ tags $ \tag ->
		writeFile ("docs/tag/" <> tag <> "/index.html") $
		makeTaggedPosts templates tag $
		tagMap M.! tag