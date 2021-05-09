{-# LANGUAGE DataKinds #-}

module URI
    ( URIBase
    , mkURIBase
    , isInternal
    , toAbsolute
    , toAbsURIs
    , getInternals
    )
where

import Control.Lens
import Text.URI ( URI, RText, RTextLabel(..), mkURI, render )
import qualified Text.URI as URI ( Authority(..) )
import Text.URI.Lens
import Control.Applicative ( (<|>) )
import Data.Text ( Text )
import Data.Maybe ( mapMaybe )

-- TODO clean up api mess

data URIBase = URIBase (RText 'Scheme) (RText 'Host)
    deriving (Show)

mkURIBase :: Text -> Maybe URIBase
mkURIBase s = do
    uri <- mkURI s
    scheme <- uri ^. uriScheme
    host <- uri ^? uriAuthority . _Right . authHost
    return $ URIBase scheme host

toAbsolute :: URIBase -> Text -> Maybe Text
toAbsolute base = fmap (render . toAbs base) . mkURI

getInternals :: URIBase -> [Text] -> [Text]
getInternals base = map render . filter (isInternal base) . map (toAbs base) . mapMaybe mkURI

toAbsURIs :: URIBase -> [Text] -> [Text]
toAbsURIs base xs = map (render . toAbs base) $ mapMaybe mkURI xs

toAbs :: URIBase -> URI -> URI
toAbs (URIBase scheme host) = addHost . addScheme . rmFragment
    where
        rmFragment = uriFragment .~ Nothing
        addScheme = uriScheme %~ (<|> Just scheme)
        addHost uri =
            -- TODO it's probably not entirely correct
            -- TODO find lens operator
            case uri ^? uriAuthority . _Right . authHost of 
                Nothing -> uri & uriAuthority .~ Right (URI.Authority Nothing host Nothing)
                _ -> uri

isInternal :: URIBase -> URI -> Bool
isInternal (URIBase _ host) uri =
    case uri ^? uriAuthority . _Right . authHost of
        Just host' -> host == host'
        _ -> True
