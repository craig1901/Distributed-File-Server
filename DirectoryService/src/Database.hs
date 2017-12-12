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

module Database where

import  Control.Monad.IO.Class  (liftIO)
import  Control.Monad.Logger    (runStderrLoggingT, runStdoutLoggingT, runNoLoggingT)
import  Control.Monad.Trans.Resource
import  Database.Persist
import  Database.Persist.MySQL  (withMySQLPool, ConnectionPool, MySQLConnectInfo, createMySQLPool, mkMySQLConnectInfo)
import  Database.Persist.Sql
import  Database.Persist.TH
import Api.File

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Files
    name String
    path String
    deriving Show
|]

conn :: MySQLConnectInfo
conn = mkMySQLConnectInfo "localhost" "root" "root" "Files"


-- makePool :: IO ConnectionPool
-- makePool = do
--     c <- conn
--     runStdoutLoggingT (createMySQLPool c 1)

runDB query = runStderrLoggingT $ withMySQLPool conn 10 $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
        runMigration migrateAll
        query
