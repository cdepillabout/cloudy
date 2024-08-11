module Cloudy.Cmd.Scaleway.Utils where

import Cloudy.Scaleway (Zone (..), zoneFromText)
import Data.Text (Text, unpack)
import Servant.API (AuthProtect)
import Servant.Client (BaseUrl (BaseUrl), Scheme (Https))
import Servant.Client.Core (mkAuthenticatedRequest, AuthenticatedRequest, AuthClientData, Request, addHeader)

createAuthReq :: Text -> AuthenticatedRequest (AuthProtect "auth-token")
createAuthReq secretKey = mkAuthenticatedRequest secretKey createAuthTokenHeader

createAuthTokenHeader :: Text -> Request -> Request
createAuthTokenHeader authData = addHeader "X-Auth-Token" authData

type instance AuthClientData (AuthProtect "auth-token") = Text

scalewayBaseUrl :: BaseUrl
scalewayBaseUrl = BaseUrl Https "api.scaleway.com" 443 ""

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
