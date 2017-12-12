{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Client where

import Api.File as F
import Api.Directory as D
import Text.Editor
import Data.ByteString.Char8 (pack)

listFiles :: IO ()
listFiles = do
    res <- D.query ls
    case res of
        Left err -> putStrLn $ "Error: " ++ show err ++ "\n\n"
        Right res' -> do
            mapM_ (\file -> do
                putStrLn $ "File: " ++ (filesName file) ++ ","
                putStrLn $ "Path: " ++ (filesPath file) ++ "\n") res'


getFile :: String -> IO ()
getFile path = do
    res <- F.query (getFile' path)
    case res of
        Left err -> putStrLn $ "Error: " ++ show err ++ "\n\n"
        Right file -> putStrLn $ fileContents file

write :: String -> IO ()
write path = do
    res <- F.query (getFile' path)
    case res of
        Left err -> putStrLn $ "Error: " ++ show err ++ "\n\n"
        Right file -> do
            contents <- runUserEditorDWIM plainTemplate (pack $ fileContents file)
            let updated = File (fileName file) (wrapStr contents)
            F.query (update updated)
            putStrLn "File updated!\n"

newFile :: String -> IO ()
newFile f = do
    contents <- runUserEditorDWIM plainTemplate ""
    let file = File f (wrapStr contents)
    F.query (putFile' file)
    res <- D.query (put' file)
    case res of
        Left err -> putStrLn $ "Error: " ++ show err ++ "\n\n"
        Right res' -> putStrLn $ show res' ++"\n\n"
