{-# LANGUAGE OverloadedStrings #-}

module Crawler where

import Text.HTML.Scalpel
import Text.URI

import Control.Monad.Reader ( ReaderT )
import Data.IORef
import Data.Maybe ( fromMaybe )
import qualified Data.Set as S


data AppState = AppState
    { links :: S.Set String
    , pending :: Int
    }

data AppConfig = AppConfig
    { baseUrl :: String
    , workers :: Int
    }

data Env = Env
    { state :: IORef AppState
    , config :: AppConfig
    , fetchLinks :: String -> IO [String]
    }

type App = ReaderT Env IO

env :: String -> Int -> (String -> IO [String]) -> IO Env
env u l f = do
    s <- newIORef AppState { links = S.singleton u, pending = 1 }
    return Env
        { state = s
        , config = AppConfig
            { baseUrl = u
            , workers = l
            }
        , fetchLinks = f
        }

crawl :: App ()
crawl = undefined

scrapeFetchLinks url = fromMaybe [] <$> scrapeURL url (attrs "href" "a")
