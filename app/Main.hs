{-# LANGUAGE RecordWildCards #-}

module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import Control.Monad.Reader (runReaderT)

import Crawler

data Opts = Opts
    { url :: String
    , limit :: Int
    } deriving (Show)

args :: Parser Opts
args = Opts
    <$> strOption
         ( long "url"
        <> short 'u'
        <> help "Url to crawl" )
    <*> option auto
         ( long "limit"
        <> help "Rate limit"
        <> showDefault
        <> value 5
        <> metavar "INT" )

main :: IO ()
main = execParser opts >>= opts2env >>= runReaderT crawl
    where
        opts = info (args <**> helper)
             ( fullDesc
            <> progDesc "Web crawler"
            <> header "webcrawl -u http://example.com -l 10" )
        opts2env Opts{..} = env url limit scrapeFetchLinks
