{-# LANGUAGE OverloadedStrings #-}
module GitHub where

import Import

import Data.Aeson ((.:))
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromJust)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import Network.HTTP.Types.Method (Method)
import System.Locale (defaultTimeLocale, iso8601DateFormat, timeFmt)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Network.HTTP.Conduit as HTTP

getUserName :: Text -> IO Text
getUserName token = do
    userContent <- HTTP.simpleHttp . Text.unpack $ "https://api.github.com/user?access_token=" `mappend` token
    let name = maybe "" jsonString $ Aeson.decode userContent >>= parseMaybe userName
    return name
        where
            userName (Object o) = o .: "name"
            userName _ = fail "object.sha missing"

getBase :: Text -> Text -> Text -> IO (Text, Text)
getBase ref repoUrl token = do
    refContent <- HTTP.simpleHttp . Text.unpack $ mconcat [repoUrl, "/git/refs/", ref, "?access_token=", token]
    let commit = jsonString . fromJust $ Aeson.decode refContent >>= parseMaybe refCommit
    commitContent <- HTTP.simpleHttp . Text.unpack $ mconcat [repoUrl, "/git/commits/", commit, "?access_token=", token]
    let tree = jsonString . fromJust $ Aeson.decode commitContent >>= parseMaybe commitTree
    return (commit, tree)
        where
            refCommit (Object o) = o .: "object" >>= (.: "sha")
            refCommit _ = fail "object.sha missing"
            commitTree (Object o) = o .: "tree" >>= (.: "sha")
            commitTree _ = fail "tree.sha missing"

{-
createBlob :: Text -> Text -> Text -> IO Text
createBlob content repoUrl token = do
    let o = HashMap.fromList [("content", Aeson.String content)]
    body <- jsonReq "POST" o (mconcat [repoUrl, "/git/blobs"]) token
    let blob = jsonString . fromJust $ Aeson.decode (HTTP.responseBody body) >>= parseMaybe bodyBlob
    return blob
        where
            bodyBlob (Object o) = o .: "sha"
            bodyBlob _ = fail "sha missing"
-}

createTree :: Text -> Text -> Text -> Text -> IO Text
createTree baseTree content repoUrl token = do
    let o = HashMap.fromList
            [ ("base_tree", Aeson.String baseTree)
            , ("tree", Aeson.Array $ Vector.fromList
                         [ Aeson.Object $ HashMap.fromList
                                            [ ("path", Aeson.String "LICENSE.txt")
                                            , ("mode", Aeson.String "100644")
                                            , ("type", Aeson.String "blob")
                                            , ("content", Aeson.String content)
                                            ]
                         ])
            ]

    body <- jsonReq "POST" o (mconcat [repoUrl, "/git/trees"]) token
    let tree = jsonString . fromJust $ Aeson.decode (HTTP.responseBody body) >>= parseMaybe bodyTree
    return tree
        where
            bodyTree (Object o) = o .: "sha"
            bodyTree _ = fail "sha missing"

createCommit :: Text -> Text -> Text -> Text -> Text -> IO Text
createCommit licenseName baseCommit tree repoUrl token = do
    time <- getCurrentTime
    let timeString = formatTime defaultTimeLocale iso8601DateTimeFormat time
        o = HashMap.fromList
            [ ("message", Aeson.String $ mconcat ["Add ", licenseName, " license"])
            , ("author", Aeson.Object $ HashMap.fromList
                           [ ("name", Aeson.String "Licentious")
                           , ("email", Aeson.String "licentious@brianmckenna.org")
                           , ("date", Aeson.String $ Text.pack timeString)
                           ])
            , ("parents", Aeson.String baseCommit)
            , ("tree", Aeson.String tree)
            ]
    print timeString
    body <- jsonReq "POST" o (mconcat [repoUrl, "/git/commits"]) token
    let commit = jsonString . fromJust $ Aeson.decode (HTTP.responseBody body) >>= parseMaybe bodyCommit
    return commit
        where
            bodyCommit (Object o) = o .: "sha"
            bodyCommit _ = fail "sha missing"

updateRef :: Text -> Text -> Text -> Text -> IO ()
updateRef commit ref repoUrl token = do
    let o = HashMap.fromList [("sha", Aeson.String commit)]
    _ <- jsonReq "PATCH" o (mconcat [repoUrl, "/git/refs/", ref]) token
    return ()

jsonReq :: Method -> Aeson.Object -> Text -> Text -> IO (HTTP.Response ByteString)
jsonReq method o url token = HTTP.withManager $ \man -> do
    request <- HTTP.parseUrl . Text.unpack $ mconcat [url, "?access_token=", token]
    let requestBody = mkJsonRequestBody o
    HTTP.httpLbs (request { HTTP.method = method, HTTP.requestBody = requestBody }) man

mkJsonRequestBody :: Aeson.Object -> HTTP.RequestBody m
mkJsonRequestBody = HTTP.RequestBodyLBS . Aeson.encode . Aeson.Object

jsonString :: Aeson.Value -> Text
jsonString (Aeson.String t) = t
jsonString _ = ""

iso8601DateTimeFormat :: String
iso8601DateTimeFormat = iso8601DateFormat . Just . (++ "Z") $ timeFmt defaultTimeLocale
