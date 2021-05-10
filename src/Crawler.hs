{-# LANGUAGE OverloadedStrings #-}

module Crawler
    ( crawl
    )
where

import Control.Lens
import qualified Data.Set as S
import Control.Monad.Reader ( MonadReader(ask), MonadIO(liftIO) )

import Control.Monad
import Control.Concurrent ( forkIO, newEmptyMVar, putMVar, takeMVar, MVar )
import Data.IORef ( readIORef, writeIORef )

import Types
import Utils ( URL, parseInternalURLs )


crawl :: App ()
crawl = do
    env <- ask
    toWorkers <- liftIO newEmptyMVar
    fromWorkers <- liftIO newEmptyMVar

    -- workers
    liftIO $ replicateM_ (env ^. config . workers) $ forkIO $ forever $ do
        takeMVar toWorkers >>= getLinks env >>= putMVar fromWorkers

    -- init links
    liftIO $ do
        state' <- readIORef (env ^. state)
        forM_ (state' ^. links) (putMVar toWorkers)

    loop toWorkers fromWorkers

    where
        getLinks e u = parseInternalURLs (e ^. config . urlBase) <$> (e ^. fetchLinks) u

loop :: MVar URL -> MVar [URL] -> App ()
loop toWorkers fromWorkers = do
    env <- ask
    cnt <- liftIO $ do
        state' <- readIORef (env ^. state)
        let links' = state' ^. links
        newLinks <- filter (`S.notMember` links') <$> takeMVar fromWorkers
        state'' <-
            if not (null newLinks)
                then do
                    _ <- forkIO $ forM_ newLinks (putMVar toWorkers) 
                    let links'' = foldr S.insert links' newLinks
                    return $ state' & (links .~ links'') . (pending +~ length newLinks - 1)
                else do
                    return $ state' & (pending -~ 1)
        writeIORef (env ^. state) state''
        return (state'' ^. pending)
    when (cnt > 0) $ loop toWorkers fromWorkers
