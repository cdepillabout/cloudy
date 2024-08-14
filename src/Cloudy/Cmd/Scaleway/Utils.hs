{-# LANGUAGE RankNTypes #-}
module Cloudy.Cmd.Scaleway.Utils where

import Cloudy.Scaleway (Zone (..), zoneFromText)
import Data.Text (Text, unpack)
import Servant.API (AuthProtect)
import Servant.Client (BaseUrl (BaseUrl), Scheme (Https), ClientM, ClientError, mkClientEnv, runClientM)
import Servant.Client.Core (mkAuthenticatedRequest, AuthenticatedRequest, AuthClientData, Request, addHeader)
import Network.HTTP.Client.TLS (newTlsManager)

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
    (Nothing, Nothing) -> pure NL1

-- fetchPaged :: 
