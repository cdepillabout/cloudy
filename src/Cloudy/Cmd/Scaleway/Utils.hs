module Cloudy.Cmd.Scaleway.Utils where

import Cloudy.Scaleway (Zone (..), zoneFromText, PageNum (PageNum))
import Data.Foldable (asum, foldl')
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.API (AuthProtect, Headers (Headers), Header, HList (..), ResponseHeader (Header))
import Servant.Client (BaseUrl (BaseUrl), Scheme (Https), ClientM, ClientError, mkClientEnv, runClientM)
import Servant.Client.Core (mkAuthenticatedRequest, AuthenticatedRequest, AuthClientData, Request, addHeader)

createAuthReq :: Text -> AuthenticatedRequest (AuthProtect "auth-token")
createAuthReq secretKey = mkAuthenticatedRequest secretKey createAuthTokenHeader

createAuthTokenHeader :: Text -> Request -> Request
createAuthTokenHeader authData = addHeader "X-Auth-Token" authData

type instance AuthClientData (AuthProtect "auth-token") = Text

scalewayBaseUrl :: BaseUrl
scalewayBaseUrl = BaseUrl Https "api.scaleway.com" 443 ""

runScalewayClientM :: (forall x. ClientError -> IO x) -> ClientM a -> IO a
runScalewayClientM errHandler action = do
  manager <- newTlsManager
  let clientEnv = mkClientEnv manager scalewayBaseUrl
  res <- runClientM action clientEnv
  case res of
    Left err -> errHandler err
    Right a -> pure a

defaultZone :: Zone
defaultZone = NL1

getZone :: Maybe Text -> Maybe Text -> IO Zone
getZone maybeZoneFromConfFile maybeZoneFromCliOpts =
  case (maybeZoneFromConfFile, maybeZoneFromCliOpts) of
    (_, Just zoneFromCliOpts) ->
      case zoneFromText zoneFromCliOpts of
        Nothing ->
          error . unpack $
            "Could not parse zone specified in --zone option on cli: " <> zoneFromCliOpts
        Just zone -> pure zone
    (Just zoneFromConfFile, _) ->
      case zoneFromText zoneFromConfFile of
        Nothing ->
          error . unpack $
            "Could not parse zone specified in scaleway.defaultZone in config file: " <> zoneFromConfFile
        Just zone -> pure zone
    (Nothing, Nothing) -> pure defaultZone

getMaybeOrDefault :: Foldable t => a -> t (Maybe a) -> a
getMaybeOrDefault defVal maybes = fromMaybe defVal (asum maybes)

defaultInstanceType :: Text
defaultInstanceType = "PLAY2-NANO"

getInstanceType :: Maybe Text -> Maybe Text -> Text
getInstanceType maybeInstanceTypeFromConfFile maybeInstanceTypeFromCliOpts =
  getMaybeOrDefault
    defaultInstanceType
    [maybeInstanceTypeFromCliOpts, maybeInstanceTypeFromConfFile]

defaultImageId :: Text
defaultImageId = "ubuntu_noble"

getImageId :: Maybe Text -> Maybe Text -> Text
getImageId maybeImageIdFromConfFile maybeImageIdFromCliOpts =
  getMaybeOrDefault
    defaultImageId
    [maybeImageIdFromCliOpts, maybeImageIdFromConfFile]

fetchPagedApi ::
  Monad m =>
  (Maybe PageNum -> m (Headers '[Header "x-total-count" Int] a)) ->
  (a -> a -> a) ->
  (a -> Int) ->
  m a
fetchPagedApi fetchPage combineResults countResultsOnPage = do
  Headers page1Res headers <- fetchPage (Just $ PageNum 1)
  let page1Count = countResultsOnPage page1Res
  totalCount <-
    case headers of
      HCons h HNil ->
        case h of
          Header totalCount -> pure totalCount
          _ -> error "fetchPagedApi: could not find or decode header x-total-count for some reason"
  if page1Count >= totalCount
    then pure page1Res
    else do
      allRes <-
        unfoldM
          (\(currTotal, pageNumToFetch) ->
            if currTotal >= totalCount
              then pure Nothing
              else do
                Headers pageRes _ <- fetchPage (Just $ PageNum pageNumToFetch)
                let newTotal = currTotal + countResultsOnPage pageRes
                    nextPageNum = pageNumToFetch + 1
                pure $ Just (pageRes, (newTotal, nextPageNum))
          )
          (page1Count, 2)
      pure $ foldl' combineResults page1Res allRes

unfoldM :: Monad m => (s -> m (Maybe (a, s))) -> s -> m [a]
unfoldM f s = do
  mres <- f s
  case mres of
    Nothing -> return []
    Just (a, s') -> fmap (a :) (unfoldM f s')
