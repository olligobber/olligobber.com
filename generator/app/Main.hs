module Main where

import System.Directory (copyFile, createDirectoryIfMissing)

data Post = Post
	{ title :: String
	, description :: String
	, path :: String
	, date :: String
	, tags :: [String]
	}

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
	copyStatic
