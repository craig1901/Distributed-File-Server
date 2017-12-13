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
import Data.Maybe
import  Api.Locking as L
import  Database

main :: IO ()
main = do
    -- res <- runDB $ updateWhere [LocksFilePath ==. "hi.txt"] [LocksIsLocked =. True]
    -- let file = entityVal $ fromJust res
    -- if not (locksIsLocked file) then print "True"
    --     else print "False"
    -- print $ show file
    runDB $ updateWhere [LocksFilePath ==. "hi.txt"] [LocksIsLocked =. True]
    return ()
