{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (fromJust)
import Data.String (fromString)
import qualified Data.Text.Lazy.IO as TIO
import Data.Time.Format (formatTime, defaultTimeLocale, parseTimeM)
import qualified Text.Atom.Feed as F
import Text.Feed.Export (textFeed)
import Text.Feed.Types (Feed(AtomFeed))

import Lib (Post(..), loadPosts)

feedEntry :: Post -> F.Entry
feedEntry post = F.Entry
	{ F.entryId = fromString $ "https://olligobber.com/" <> path post
	, F.entryTitle = F.TextString $ fromString $ title post
	, F.entryUpdated = fromString $ formatTime defaultTimeLocale "%Y-%m-%dT00:00:00Z" $ date post
	, F.entryAuthors = [F.Person "olligobber" Nothing Nothing []]
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
	, F.feedUpdated = fromString $ formatTime defaultTimeLocale "%Y-%m-%dT00:00:00Z" $ date $ head posts
	, F.feedAuthors = []
	, F.feedCategories = []
	, F.feedContributors = []
	, F.feedGenerator = Nothing
	, F.feedIcon = Just "https://olligobber.com/Logo.png"
	, F.feedLinks =
		[ F.nullLink "https://olligobber.com"
		, F.Link "https://olligobber.com/atom.xml" (Just $ Left "self") Nothing Nothing Nothing Nothing [] []
		]
	, F.feedLogo = Just "https://olligobber.com/Logo.png"
	, F.feedRights = Nothing
	, F.feedSubtitle = Nothing
	, F.feedEntries = feedEntry <$> posts
	, F.feedAttrs = []
	, F.feedOther = []
	}

main :: IO ()
main = do
	posts <- loadPosts
	TIO.writeFile "docs/atom.xml" $ fromJust $ textFeed $ AtomFeed $ feed posts