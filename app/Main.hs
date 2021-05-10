{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.Reader ( runReaderT )

import Options.Applicative
import Data.Text ( Text )

import App ( app, env )
import Utils ( scrapeFetchLinks )


data Opts = Opts
    { url :: Text
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
main = execParser opts >>= opts2env >>= runReaderT app
    where
        opts = info (args <**> helper)
             ( fullDesc
            <> progDesc "Web crawler"
            <> header "webcrawl -u http://example.com -l 10" )
        opts2env Opts{..} = env url limit scrapeFetchLinks
