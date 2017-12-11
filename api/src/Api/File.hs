{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Api.File where

import Data.Aeson
import Data.Aeson.TH
import Data.Proxy
import Network.Wai
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Client
import Network.HTTP.Client (newManager, defaultManagerSettings)
import GHC.Generics

type FileApi = Capture "filepath" String :> Get '[JSON] File
          :<|> ReqBody '[JSON] File :> Post '[JSON] FilePost

fileApi :: Proxy FileApi
fileApi = Proxy

data File = File
    {
        fileName :: String,
        fileContents :: String
    } deriving(Show, Generic)

instance ToJSON File
instance FromJSON File

data FilePost = FilePost
    {
        saved :: Bool
    }deriving(Show, Generic)

instance ToJSON FilePost
instance FromJSON FilePost

getFile' :: String -> ClientM File
putFile' :: File -> ClientM FilePost

getFile' :<|> putFile' = client fileApi


query :: Show a =>  ClientM a -> IO ()
query f = do
    manager <- newManager defaultManagerSettings
    result <- runClientM f (ClientEnv manager (BaseUrl Http "localhost" 8080 ""))
    case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right res -> putStrLn $ show res ++"\n"
