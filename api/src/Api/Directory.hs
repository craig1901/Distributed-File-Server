{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeFamilies               #-}

module Api.Directory where

import Data.Aeson
import Data.Aeson.TH
import Data.Proxy
import Network.Wai
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Client
import Database.Persist
import Database.Persist.MySQL  (ConnectionPool, MySQLConnectInfo, createMySQLPool, mkMySQLConnectInfo)
import Database.Persist.Sql
import Database.Persist.TH
import Network.HTTP.Client (newManager, defaultManagerSettings)
import GHC.Generics
import Api.File

type DirectoryApi = ReqBody '[JSON] File :> Post '[JSON] ()

directoryApi :: Proxy DirectoryApi
directoryApi = Proxy

put' :: File -> ClientM ()
put' = client directoryApi


query :: Show a => ClientM a -> IO ()
query f = do
    manager <- newManager defaultManagerSettings
    result <- runClientM f (ClientEnv manager (BaseUrl Http "localhost" 5000 ""))
    case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right res -> putStrLn $ show res ++"\n"
