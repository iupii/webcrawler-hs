module App
    ( app
    , env
    )
where

import Control.Lens
import qualified Data.Set as S
import Control.Monad.Reader

import Control.Exception ( throwIO )
import Data.IORef ( newIORef, readIORef )
import Data.Text ( Text )

import Types
import Crawler ( crawl )
import Utils ( parseURLBase )

env :: Text -> Int -> (Text -> IO [Text]) -> IO Env
env u l f =
    case parseURLBase u of
        Just (urlBase', links') -> do
            s <- newIORef AppState
                { _links = S.fromList links'
                , _pending = length links'
                }
            return Env
                { _state = s
                , _config = AppConfig
                    { _urlBase = urlBase'
                    , _workers = l
                    }
                , _fetchLinks = f
                }
        Nothing -> throwIO (BadUrlException u)

app :: App ()
app = crawl >> out

-- TODO pretty print
out :: App ()
out = do
    env' <- ask
    liftIO $ do
        print "links: "
        state' <- readIORef (env' ^. state)
        forM_ (state' ^. links) print
