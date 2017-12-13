{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
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
import Api.Locking
import Api.File
import Database
import System.Directory
import System.FilePath



startApp :: IO ()
startApp = run 5555 app

app :: Application
app = serve api server

api :: Proxy LockingApi
api = Proxy

server :: Server LockingApi
server = check :<|> lock :<|> unlock

check :: String -> Handler LockCheck
check path = do
    res <- runDB $ selectFirst [LocksFilePath ==. path] []
    let file = entityVal $ fromJust res
    if (locksIsLocked file) then return $ LockCheck True
        else return $ LockCheck False
    -- liftIO $ print $ show file
    -- return $ LockCheck True

lock :: String -> Handler ()
lock path = do
    runDB $ updateWhere [LocksFilePath ==. path] [LocksIsLocked =. True]
    return ()

unlock :: String -> Handler ()
unlock path = do
    runDB $ updateWhere [LocksFilePath ==. path] [LocksIsLocked =. False]
    return ()
