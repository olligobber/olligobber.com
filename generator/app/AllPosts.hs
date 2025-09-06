{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Lib
	( Post, PostDivTemplates
	, loadPostDivTemplates, makePostDiv, substitute, loadPosts
	)

data Templates = Templates
	{ allPosts :: String
	, postDiv :: PostDivTemplates
	}

loadTemplates :: IO Templates
loadTemplates = do
	allPosts <- readFile "templates/all_posts.html"
	postDiv <- loadPostDivTemplates
	pure $ Templates {allPosts, postDiv}

makeAllPosts :: Templates -> [Post] -> String
makeAllPosts templates posts =
	substitute "{posts}" postsList $ allPosts templates where
		postsList = foldMap (makePostDiv $ postDiv templates) posts

main :: IO ()
main = do
	posts <- loadPosts
	templates <- loadTemplates
	writeFile "docs/posts/index.html" $ makeAllPosts templates posts