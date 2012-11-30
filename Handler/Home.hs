{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import AuthGitHub
import GitHub
import Yesod.Auth

import Data.Aeson ((.:))
import Data.Aeson.Types (parseMaybe)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import Data.Traversable (sequenceA)
import System.Locale (defaultTimeLocale)
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Vector as Vector
import qualified Network.HTTP.Conduit as HTTP
import qualified System.FilePath.Posix as Posix

data Repository = Repository { repoName :: Text, repoHtmlUrl :: Text, repoApiUrl :: Text } deriving Eq

data License = License {licenseName :: Text, licenseContent :: Text } deriving Eq

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ do
    setTitle "Licentious"
    $(widgetFile "homepage")


getReposR :: Handler RepHtml
getReposR = do
    token <- requireAuthId
    name <- liftIO $ getUserName token
    (repoWidget, enctype) <- generateFormPost . repoForm $ Just name
    defaultLayout $ do
        setTitle "Licentious"
        $(widgetFile "repos")

postReposR :: Handler RepHtml
postReposR = do
    token <- requireAuthId
    ((result, repoWidget), enctype) <- runFormPost $ repoForm Nothing
    case result of
        (FormSuccess (repo, license, name)) -> do
            -- TODO: Figure out why API not returning default_branch
            time <- liftIO getCurrentTime
            
            let repoBranch _ = "heads/master"
                year = Text.pack $ formatTime defaultTimeLocale "%Y" time
                licenseContent' = Text.replace "$year" year . Text.replace "$name" name $ licenseContent license

            liftIO $ do
                (baseCommit, baseTree) <- getBase (repoBranch repo) (repoApiUrl repo) token
                tree <- createTree baseTree licenseContent' (repoApiUrl repo) token
                commit <- createCommit (licenseName license) baseCommit tree (repoApiUrl repo) token
                updateRef commit (repoBranch repo) (repoApiUrl repo) token

            redirect $ repoHtmlUrl repo
        _ -> defaultLayout $ do
            setTitle "Licentious"
            $(widgetFile "repos")


repoForm :: Maybe Text -> Form (Repository, License, Text)
repoForm maybeName = renderBootstrap $ (,,)
    <$> areq (selectField gitHubRepositoriesSelectField) (fieldSettingsLabel MsgRepository) Nothing
    <*> areq (selectField licensesSelectField) (fieldSettingsLabel MsgLicense) Nothing
    <*> areq textField (fieldSettingsLabel MsgName) maybeName


licensesSelectField :: Handler (OptionList License)
licensesSelectField = do
    licenses <- liftIO licensesContents
    optionsPairs licenses

licensesContents :: IO [(Text, License)]
licensesContents = sequenceA $ map nameWithContent licensesNames
    where
        nameWithContent license = do
            content <- TextIO.readFile . Posix.combine staticDir $ fst license
            return (snd license, License (snd license) content)

licensesNames :: [(FilePath, Text)]
licensesNames =
    [ ("mit.txt", "MIT")
    , ("apache.txt", "Apache 2.0")
    , ("CC_BY-NC-SA_3.0.txt", "CC BY-NC-SA 3.0")
    , ("newbsd.txt", "BSD (3 clause)")
    , ("freebsd.txt", "FreeBSD (2 clause)")
    , ("gnu-gplv3.txt", "GNU GPL v3")
    ]

gitHubRepositoriesSelectField :: Handler (OptionList Repository)
gitHubRepositoriesSelectField = do
    token <- requireAuthId
    repositories <- liftIO . fmap (fromMaybe []) $ getGitHubRepositories token
    optionsPairs $ fmap (\r -> (repoName r, r)) repositories

getGitHubRepositories :: Text -> IO (Maybe [Repository])
getGitHubRepositories token = do
    response <- HTTP.simpleHttp $ "https://api.github.com/user/repos?per_page=200&access_token=" ++ Text.unpack token
    let repositories = Aeson.decode response :: Maybe Aeson.Array
    return $ repositories >>= sequenceA . Vector.toList . fmap (parseMaybe extractFields)
        where extractFields (Object o) = Repository <$> o .: "name" <*> o .: "html_url" <*> o .: "url"
              extractFields _ = fail "Expected repository"
