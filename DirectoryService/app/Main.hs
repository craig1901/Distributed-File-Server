{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import  Control.Monad.IO.Class  (liftIO)
import  Control.Monad.Logger    (runStderrLoggingT)
import  Database.Persist
import  Database.Persist.MySQL
import  Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
File
    name String
    path String
    deriving Show
|]

connectionInfo = defaultConnectInfo { connectHost = "localhost",
                                      connectUser = "root",
                                      connectPort = 3306,
                                      connectPassword = "root",
                                      connectDatabase = "Files"}

main :: IO ()
main = runStderrLoggingT $ withMySQLPool connectionInfo 10 $ \pool -> liftIO $ do
         flip runSqlPersistMPool pool $ do
           runMigration migrateAll
