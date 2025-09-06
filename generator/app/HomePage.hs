{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Lib
	( Post, PostDivTemplates
	, loadPostDivTemplates, makePostDiv, substitute, loadPosts
	)

data Templates = Templates
	{ homePage :: String
	, postDiv :: PostDivTemplates
	}

loadTemplates :: IO Templates
loadTemplates = do
	homePage <- readFile "templates/home_page.html"
	postDiv <- loadPostDivTemplates
	pure $ Templates {homePage, postDiv}

makeHomePage :: Templates -> [Post] -> String
makeHomePage templates posts =
	substitute "{posts}" recentPosts $ homePage templates where
		recentPosts = foldMap (makePostDiv $ postDiv templates) $ take 3 posts

main :: IO ()
main = do
	posts <- loadPosts
	templates <- loadTemplates
	writeFile "docs/index.html" $ makeHomePage templates posts