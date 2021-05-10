{-# LANGUAGE OverloadedStrings #-}

module Utils
    ( URL
    , URLBase
    , parseURLBase
    , scrapeFetchLinks
    , parseInternalURLs
    )
where

import Text.URI ( mkURI, render )
import Text.HTML.Scalpel ( scrapeURL, attrs )

import Data.Maybe ( fromMaybe, mapMaybe )
import Data.Text ( Text, unpack )

import Utils.URL ( URL, URLBase, parseURLBase )
import qualified Utils.URL as URL ( toAbsolute, isInternal, isParsable )


-- TODO handle exceptions
scrapeFetchLinks :: URL -> IO [Text]
scrapeFetchLinks url = fromMaybe [] <$> scrapeURL (unpack url) (attrs "href" "a")

parseInternalURLs :: URLBase -> [Text] -> [URL]
parseInternalURLs base = map render . filter (\u -> URL.isParsable u && URL.isInternal base u) . map (URL.toAbsolute base) . mapMaybe mkURI
