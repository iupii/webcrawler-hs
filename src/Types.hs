{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens ( makeLenses )
import qualified Data.Set as S
import Control.Monad.Reader ( ReaderT )

import Control.Exception ( Exception )
import Data.Typeable ( Typeable )
import Data.IORef ( IORef )
import Data.Text ( Text )

import Utils ( URLBase, URL )

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

data CrawlerException = BadUrlException URL
    deriving (Typeable, Show)

instance Exception CrawlerException

makeLenses ''AppState
makeLenses ''AppConfig
makeLenses ''Env
