{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
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
import Api.File as F

runServer :: IO ()
runServer = run 8080 app

app :: Application
app = serve api server

api :: Proxy FileApi
api = Proxy

server :: Server FileApi
server = getFile
    :<|> putFile

getFile :: String -> Handler File
getFile f = do
    let path = "files/" ++ f
    exists <- liftIO $ doesFileExist path
    if exists
        then do
            contents <- liftIO $ readFile path
            liftIO $ putStr contents
            return $ File path contents
        else throwError noFileExists

putFile :: File -> Handler FilePost
putFile f = do
    let path = "files/" ++ (fileName f)
    exists <- liftIO $ doesFileExist path
    if exists
        then throwError fileAlreadyExists
        else do
            liftIO $ writeFile path (fileContents f)
            return $ FilePost True




noFileExists = err404 {errBody = "File Doesn't exist on this path"}
fileAlreadyExists = err400 { errBody = "File already exists in this directory"}
