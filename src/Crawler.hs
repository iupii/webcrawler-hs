{-# LANGUAGE OverloadedStrings #-}

module Crawler
    ( env
    , crawl
    , scrapeFetchLinks
    )
where

import Control.Monad.Reader ( ReaderT )
import Data.IORef
import Data.Maybe ( fromJust )
import Data.Text ( Text )
import qualified Data.Set as S

import Utils

data AppState = AppState
    { links :: S.Set Text
    , pending :: Int
    }

data AppConfig = AppConfig
    { uriBase :: URIBase
    , workers :: Int
    }

data Env = Env
    { state :: IORef AppState
    , config :: AppConfig
    , fetchLinks :: String -> IO [Text]
    }

type App = ReaderT Env IO

env :: Text -> Int -> (String -> IO [Text]) -> IO Env
env u l f = do
    s <- newIORef AppState { links = S.singleton u, pending = 1 }
    return Env
        { state = s
        , config = AppConfig
            { uriBase = fromJust $ mkURIBase u
            , workers = l
            }
        , fetchLinks = f
        }

crawl :: App ()
crawl = undefined


