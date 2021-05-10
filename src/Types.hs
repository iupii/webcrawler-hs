{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Exception ( Exception )
import Control.Monad.Reader ( ReaderT )
import Control.Lens

import Data.IORef ( IORef )
import Data.Text ( Text )
import Data.Typeable
import qualified Data.Set as S

import Utils

data AppState = AppState
    { _links :: S.Set Text
    , _pending :: Int
    }
    deriving (Show)

data AppConfig = AppConfig
    { _urlBase :: URLBase
    , _workers :: Int
    }

data Env = Env
    { _state :: IORef AppState
    , _config :: AppConfig
    , _fetchLinks :: URL -> IO [Text]
    }

type App = ReaderT Env IO

data CrawlerException = BadUrlException Text
    deriving (Typeable, Show)

instance Exception CrawlerException

makeLenses ''AppState
makeLenses ''AppConfig
makeLenses ''Env
