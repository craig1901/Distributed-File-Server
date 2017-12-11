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

module Main where

import  Control.Monad.IO.Class  (liftIO)
import  Control.Monad.Logger    (runStderrLoggingT, runStdoutLoggingT, runNoLoggingT)
import  Database.Persist
import  Database.Persist.MySQL  (ConnectionPool, MySQLConnectInfo, createMySQLPool, mkMySQLConnectInfo)
import  Database.Persist.Sql
import  Database.Persist.TH
import System.Environment       (lookupEnv)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
File
    name String
    path String
    deriving Show
|]

conn :: IO MySQLConnectInfo
conn = return (mkMySQLConnectInfo "localhost" "root" "root" "Files")

makePool :: IO ConnectionPool
makePool = do
    c <- conn
    runStdoutLoggingT (createMySQLPool c 1)


main :: IO ()
main = do
    p <- makePool
    runSqlPool (runMigration migrateAll) p


-- main = runStderrLoggingT $ withMySQLPool connectionInfo 10 $ \pool -> liftIO $ do
--          flip runSqlPersistMPool pool $ do
--            runMigration migrateAll
