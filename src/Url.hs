{-# LANGUAGE DataKinds #-}

module Url
    ( mkBaseURI
    , toAbsURIs
    , isInternal
    , toAbs
    )
where

import Control.Lens
import Text.URI (URI, RText, RTextLabel(..), mkURI)
import qualified Text.URI as URI (Authority(..))
import Text.URI.Lens
import Control.Applicative ((<|>))
import Data.Text (Text)
import Data.Maybe (mapMaybe)

data BaseURI = BaseURI (RText 'Scheme) (RText 'Host)
    deriving (Show)

mkBaseURI :: Text -> Maybe BaseURI
mkBaseURI s = do
    uri <- mkURI s
    scheme <- uri ^. uriScheme
    host <- uri ^? uriAuthority . _Right . authHost
    return $ BaseURI scheme host

toAbsURIs :: BaseURI -> [Text] -> [URI]
toAbsURIs base xs = map (toAbs base) $ mapMaybe mkURI xs

toAbs :: BaseURI -> URI -> URI
toAbs (BaseURI scheme host) = addHost . addScheme . rmFragment
    where
        rmFragment uri = uri & uriFragment .~ Nothing
        addScheme uri = uri & uriScheme %~ (<|> Just scheme)
        addHost uri =
            -- TODO it's probably not correct
            -- TODO find lens operator
            case uri ^? uriAuthority . _Right . authHost of 
                Nothing -> uri & uriAuthority .~ Right (URI.Authority Nothing host Nothing)
                _ -> uri

isInternal :: BaseURI -> URI -> Bool
isInternal (BaseURI _ host) uri =
    case uri ^? uriAuthority . _Right . authHost of
        Just host' -> host == host'
        _ -> True
