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

module Server where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Logger    (runStderrLoggingT, runStdoutLoggingT, runNoLoggingT)
import Database.Persist
import Database.Persist.MySQL  (withMySQLPool, ConnectionPool, MySQLConnectInfo, createMySQLPool, mkMySQLConnectInfo)
import Database.Persist.Sql
import Database.Persist.TH
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List hiding(insert)
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import System.FilePath
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html
import Api.Directory
import Api.File
import Database
import System.Directory
import System.FilePath
import Data.Time


runServer :: IO ()
runServer = run 5000 app

app :: Application
app = serve api server

api :: Proxy DirectoryApi
api = Proxy

server :: Server DirectoryApi
server = listFiles :<|> put :<|> updateTime

listFiles :: Handler [Files]
listFiles = do
    files <- runDB $ selectList [] []
    return $ map entityVal files

put :: File -> Handler ()
put f = do
    liftIO $ insertFile f
    return ()

insertFile :: File -> IO ()
insertFile f = do
    let name = takeFileName (fileName f)
    let path = fileName f
    time <- getCurrentTime
    runDB (insert $ Files name path time)
    return ()

updateTime :: File -> Handler ()
updateTime f = do
    liftIO $ print "Changing the write time!"
    time <- liftIO $ getCurrentTime
    let path = fileName f
    let name = joinPath $ tail (splitPath path)
    runDB $ updateWhere [FilesPath ==. name] [FilesLastWriteTime =. time]
    return ()
