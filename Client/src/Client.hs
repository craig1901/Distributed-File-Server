{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Client where

import Api.File as F
import Api.Directory as D
import Api.Locking as L
import Text.Editor
import Data.ByteString.Char8 (pack)
import Data.Time
import Data.Cache as C



listFiles :: Cache String (String, UTCTime) -> IO ()
listFiles cache = do
    time <- getCurrentTime
    insert cache "Filename" ("fileContents", time)
    res <- D.query ls
    case res of
        Left err -> putStrLn $ "Error: " ++ show err ++ "\n\n"
        Right res' -> do
            mapM_ (\file -> do
                putStrLn $ "File: " ++ (filesName file) ++ ","
                putStrLn $ "Path: " ++ (filesPath file) ++ "\n") res'


getFile :: String -> Cache String (String, UTCTime) -> IO ()
getFile path cache = do
    k <- keys cache
    putStrLn $ show k
    res <- F.query (getFile' path)
    case res of
        Left err -> putStrLn $ "Error: " ++ show err ++ "\n\n"
        Right file -> putStrLn $ fileContents file

write :: String -> IO ()
write path = do
    locked <- L.query(checkFile' path)
    case locked of
        Left err -> putStrLn $ "Error " ++ show err ++ "\n"
        Right lockCheck -> writeIfAvailable lockCheck path

writeIfAvailable :: LockCheck -> String -> IO ()
writeIfAvailable check path = do
    case (locked check) of
        False -> do
            L.query (lock' path)
            res <- F.query (getFile' path)
            case res of
                Left err -> putStrLn $ "Error: " ++ show err ++ "\n\n"
                Right file -> do
                    contents <- runUserEditorDWIM plainTemplate (pack $ fileContents file)
                    let updated = File (fileName file) (wrapStr contents)
                    F.query (update updated)
                    D.query (update' updated)
                    L.query (unlock' path)
                    putStrLn "File updated!\n"

        True -> putStrLn "This file is locked, please try again later when it's lock is released.\n"

newFile :: String -> IO ()
newFile f = do
    contents <- runUserEditorDWIM plainTemplate ""
    let file = File f (wrapStr contents)
    L.query (putFile file)
    F.query (putFile' file)
    res <- D.query (put' file)
    case res of
        Left err -> putStrLn $ "Error: " ++ show err ++ "\n\n"
        Right res' -> putStrLn $ show res' ++"\n\n"
