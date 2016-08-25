{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Handler.Home where

import qualified Data.Text         as T
import           Import
import           Yesod.Auth.HashDB
--import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
--                              withSmallInput)
--import Text.Julius (RawJS (..))

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.

data Ok = Ok Text deriving (Generic, ToJSON)

type Password = Text

type PhoneNumber = Text

data RegisterPost = RegisterPost { username    :: Text
                                 , password    :: Password
                                 , phoneNumber :: PhoneNumber
                                 } deriving (Show)

instance FromJSON RegisterPost where
  parseJSON (Object o) = RegisterPost
    <$> o .: "username"
    <*> o .: "password"
    <*> o .: "phoneNumber"
  parseJSON _ = mzero

getHomeR :: Handler Value
getHomeR = do
  auth <- requireAuth
  let username = userIdent . entityVal $ auth
  returnJson . Ok $ "You are signed in as " ++ username

getNotLoggedInR :: Handler Value
getNotLoggedInR = returnJson $ Ok "Please log in"

postBoopLoginR :: Handler Value
postBoopLoginR = do
    regInfo <- requireJsonBody :: Handler RegisterPost
    setCreds False $ toCreds regInfo BoopLogin
    auth <- maybeAuth
    returnJson $ maybe (Ok "Invalid credentials") (\_ -> Ok $ "Logged in success! as " ++ username regInfo) auth

postRegisterR :: Handler Value
postRegisterR = do
    regInfo <- requireJsonBody :: Handler RegisterPost
    setCreds False $ toCreds regInfo BoopRegister
    auth <- maybeAuth
    returnJson $ maybe (Ok "The user already exists") (\_ -> Ok "success") auth

toCreds :: RegisterPost -> AuthType -> Creds m
toCreds regInfo auth = Creds { credsIdent = username regInfo
                        , credsPlugin = "BoopAuth"
                        , credsExtra = [
                            ("password", password regInfo)
                          , ("phoneNumber", phoneNumber regInfo)
                          , ("regType", T.pack $ show auth)
                          ]
                        }


    --setCreds True Creds { credsIdent = testUsername, credsPlugin = "BoopAuth", credsExtra = []}

--    (formWidget, formEnctype) <- generateFormPost sampleForm
--    let submission = Nothing :: Maybe (FileInfo, Text)
--        handlerName = "getHomeR" :: Text
--    defaultLayout $ do
--        let (commentFormId, commentTextareaId, commentListId) = commentIds
--        aDomId <- newIdent
--        setTitle "Welcome To Yesod!"
--        $(widgetFile "homepage")

-- postHomeR :: Handler Html
-- postHomeR = do
--     ((result, formWidget), formEnctype) <- runFormPost sampleForm
--     let handlerName = "postHomeR" :: Text
--         submission = case result of
--             FormSuccess res -> Just res
--             _ -> Nothing
--
--     defaultLayout $ do
--         let (commentFormId, commentTextareaId, commentListId) = commentIds
--         aDomId <- newIdent
--         setTitle "Welcome To Yesod!"
--         $(widgetFile "homepage")
--
-- sampleForm :: Form (FileInfo, Text)
-- sampleForm = renderBootstrap3 BootstrapBasicForm $ (,)
--     <$> fileAFormReq "Choose a file"
--     <*> areq textField (withSmallInput "What's on the file?") Nothing
--
-- commentIds :: (Text, Text, Text)
-- commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")
