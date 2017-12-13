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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeFamilies               #-}

module Api.Locking where

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

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Locks json
    filePath String
    isLocked Bool
    deriving Show
|]

type LockingApi = Capture "filePath" String :> Get '[JSON] LockCheck
             :<|> "lock" :> Capture "filePath" String :> Get '[JSON] ()
             :<|> "unlock" :> Capture "filePath" String :>  Get '[JSON] ()
             :<|> ReqBody '[JSON] File :> Post '[JSON] ()

lockingApi :: Proxy LockingApi
lockingApi = Proxy

data LockCheck = LockCheck
    {
        locked :: Bool
    }deriving(Show, Generic)

instance ToJSON LockCheck
instance FromJSON LockCheck


checkFile' :: String -> ClientM LockCheck
lock' :: String -> ClientM ()
unlock' :: String -> ClientM ()
putFile :: File -> ClientM ()

checkFile' :<|> lock' :<|> unlock' :<|> putFile = client lockingApi

query f = do
    manager <- newManager defaultManagerSettings
    runClientM f (ClientEnv manager (BaseUrl Http "localhost" 8888 ""))
