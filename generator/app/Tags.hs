{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Data.Map as M

import Lib (substitute, loadPosts, collectTags)

data Templates = Templates
	{ allTags :: String
	, tagBig :: String
	}

loadTemplates :: IO Templates
loadTemplates = do
	allTags <- readFile "templates/all_tags.html"
	tagBig <- readFile "templates/tag_big.html"
	pure $ Templates {allTags, tagBig}

makeBigTag :: Templates -> String -> String
makeBigTag templates tag =
	substitute "{url}" ("/tag/" <> tag) $
	substitute "{name}" tag $
	tagBig templates

makeAllTags :: Templates -> [String] -> String
makeAllTags templates tags =
	substitute "{tags}" (foldMap (makeBigTag templates) tags) $
	allTags templates

main :: IO ()
main = do
	tags <- M.keys . collectTags <$> loadPosts
	templates <- loadTemplates
	writeFile "docs/tags/index.html" $ makeAllTags templates tags