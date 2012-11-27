{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}
module AuthGitHub
    ( authOAuth2
    , oauth2Url
    , authGitHub
    , gitHubUrl
    , module Network.OAuth2.OAuth2
    ) where

import Prelude hiding (concat)
import Yesod (liftIO)
import Yesod.Auth
import Yesod.Form
import Yesod.Handler
import Yesod.Widget
import Network.HTTP.Types (parseQuery)
import Network.OAuth2.OAuth2
import Network.OAuth2.HTTP.HttpClient
import Control.Monad (join)
import Data.Maybe (fromJust, isJust)
import Data.Monoid (mappend)
import Data.Text (Text)
import Data.ByteString (ByteString, concat)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (toChunks)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as M

oauth2Url :: Text -> AuthRoute
oauth2Url name = PluginR name ["forward"]

authOAuth2 :: YesodAuth m
           => OAuth2                       -- ^ 'OAuth2' data-type for signing.
           -> (AccessToken -> IO (Creds m)) -- ^ How to extract ident.
           -> AuthPlugin m
authOAuth2 oauth mkCreds = AuthPlugin name dispatch login
  where
    name = "github" :: Text
    url = PluginR name []
    dispatch "GET" ["forward"] = do
      render <- getUrlRender
      tm <- getRouteToMaster
      let oauth' = oauth { oauthCallback = Just $ encodeUtf8 $ render $ tm url }
      -- TODO: add a session secret
      liftIO . putStrLn . ("Redirecting to: " ++) . unpack $ authorizationUrl oauth'
      redirect . (`mappend` "&scope=public_repo") . decodeUtf8With lenientDecode $ authorizationUrl oauth'
    dispatch "GET" [] = do
      code <- runInputGet $ ireq textField "code"

      -- Can't use the following because hoauth2 doesn't send
      -- Content-Type: application/json
      --maybeToken <- liftIO $ requestAccessToken oauth $ encodeUtf8 code

      -- GitHub returns a querystring when not asked for JSON
      let urlData = accessTokenUrl oauth $ encodeUtf8 code
      queryString <- liftIO $ doSimplePostRequest urlData
      let query = parseQuery . concat . toChunks $ queryString
          queryAccessToken = join $ lookup "access_token" query
          queryRefreshToken = join $ lookup "refresh_token" query
          maybeToken = fmap (\t -> AccessToken t queryRefreshToken) queryAccessToken

      case maybeToken of
        Just token -> do
          creds <- liftIO $ mkCreds token
          liftIO $ putStrLn "Setting credentials. Done"
          setCreds True creds
        Nothing -> permissionDenied "Rejected by GitHub"
    dispatch _ _ = notFound
    login tm = do
        render <- lift getUrlRender
        let oaUrl = render $ tm $ oauth2Url name
        [whamlet| <a href=#{oaUrl}>Login via #{name} |]

authGitHub :: YesodAuth m
           => ByteString -- ^ Consumer Key
           -> ByteString -- ^ Consumer Secret
           -> AuthPlugin m
authGitHub key secret = authOAuth2
               (OAuth2 { oauthClientId            = key
                       , oauthClientSecret        = secret
                       , oauthOAuthorizeEndpoint  = "https://github.com/login/oauth/authorize"
                       , oauthAccessTokenEndpoint = "https://github.com/login/oauth/access_token"
                       , oauthCallback            = Nothing
                       , oauthAccessToken         = Nothing
                       })
               extractCreds
  where
    extractCreds (AccessToken token _) = do
        maybeBody <- liftIO . getUser $ unpack token
        let username = fromJust $ maybeBody >>= M.lookup "login" >>= jsonString
            extra = ("access_token", decodeUtf8With lenientDecode token) : (M.toList . mapMaybe jsonString $ fromJust maybeBody)

        return $ Creds "github" username extra

getUser :: String -> IO (Maybe A.Object)
getUser token = doJSONGetRequest $ "https://api.github.com/user?access_token=" ++ token

mapMaybe :: (a -> Maybe b) -> M.HashMap k a -> M.HashMap k b
mapMaybe f = M.map fromJust . M.filter isJust . M.map f

jsonString :: A.Value -> Maybe Text
jsonString (A.String t) = Just t
jsonString A.Null = Just ""
jsonString _ = Nothing

gitHubUrl :: AuthRoute
gitHubUrl = oauth2Url "github"
