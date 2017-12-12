{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Client where

import Api.File as F
import Api.Directory as D
import Text.Editor

listFiles :: IO ()
listFiles = do
    D.query ls

getFile :: String -> IO ()
getFile path = do
    F.query (getFile' path)

newFile :: String -> IO ()
newFile f = do
    contents <- runUserEditorDWIM plainTemplate ""
    let file = File f (wrapStr contents)
    F.query (putFile' file)
    D.query (put' file)
