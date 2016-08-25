{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Handler.Home where

import qualified Data.Text         as T
import           Import
import           Yesod.Auth.HashDB

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
                        
