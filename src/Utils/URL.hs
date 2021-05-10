{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils.URL
    ( URL
    , URLBase
    , parseURLBase
    , toAbsolute
    , isInternal
    , isParsable
    )
where

import Control.Lens
import Text.URI ( URI, RText, RTextLabel(..), mkURI, mkScheme, render )
import qualified Text.URI as URI ( Authority(..) )
import Text.URI.Lens
import Control.Applicative ( (<|>) )
import Data.List ( nub )
import Data.Text ( Text )


type URL = Text

data URLBase = URLBase (RText 'Scheme) (RText 'Host)
    deriving (Show)

-- TODO check isParsable
parseURLBase :: URL -> Maybe (URLBase, [URL])
parseURLBase s = do
    uri <- mkURI s
    scheme <- uri ^. uriScheme
    host <- uri ^? uriAuthority . _Right . authHost
    return (URLBase scheme host, nub $ map (render . rmFragment) [rmQuery . rmPath $ uri, uri])
    where
        rmFragment = uriFragment .~ Nothing
        rmQuery = uriQuery .~ []
        rmPath = uriPath .~ []

toAbsolute :: URLBase -> URI -> URI
toAbsolute (URLBase scheme host) = addHost . addScheme . rmFragment
    where
        rmFragment = uriFragment .~ Nothing
        addScheme = uriScheme %~ (<|> Just scheme)
        addHost uri =
            -- TODO it's probably not entirely correct
            -- TODO find lens operator
            case uri ^? uriAuthority . _Right . authHost of 
                Nothing -> uri & uriAuthority .~ Right (URI.Authority Nothing host Nothing)
                _ -> uri

isInternal :: URLBase -> URI -> Bool
isInternal (URLBase _ host) uri =
    case uri ^? uriAuthority . _Right . authHost of
        Just host' -> host == host'
        _ -> True

isParsable :: URI -> Bool
isParsable uri = scheme == https || scheme == http
    where
        scheme = uri ^. uriScheme
        https = mkScheme "https"
        http = mkScheme "http"
