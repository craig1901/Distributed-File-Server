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
import Data.Time

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Files json
    name String
    path String
    lastWriteTime UTCTime
    deriving Show
|]

type DirectoryApi = "ls" :> Get '[JSON] [Files]
               :<|> ReqBody '[JSON] File :> Post '[JSON] ()
               :<|> "updateTime" :> ReqBody '[JSON] File :> Post '[JSON] ()

directoryApi :: Proxy DirectoryApi
directoryApi = Proxy

ls :: ClientM [Files]
put' :: File -> ClientM ()
ls :<|> put' :<|> update' = client directoryApi

query f = do
    manager <- newManager defaultManagerSettings
    runClientM f (ClientEnv manager (BaseUrl Http "localhost" 5000 ""))
