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



listFiles :: IO ()
listFiles = do
    res <- D.query ls
    case res of
        Left err -> putStrLn $ "Error: " ++ show err ++ "\n"
        Right res' -> do
            mapM_ (\file -> do
                putStrLn $ "File: " ++ (filesName file) ++ ","
                putStrLn $ "Path: " ++ (filesPath file) ++ "\n") res'


getFile :: String -> Cache String (String, UTCTime) -> IO ()
getFile path cache = do
    file <- C.lookup cache path
    case file of
        Just (f, t) -> do
            bRes <- D.query (check path t)
            case bRes of
                Right bool -> if bool == True then putStrLn f
                    else getFromServer path cache
                Left err -> putStrLn $ "Error: " ++ show err ++ "\n"
        Nothing -> getFromServer path cache

getFromServer :: String -> Cache String (String, UTCTime) -> IO ()
getFromServer path cache = do
    res <- F.query (getFile' path)
    case res of
        Left err -> putStrLn $ "Error: " ++ show err ++ "\n"
        Right file -> do
            putStrLn $ fileContents file
            cacheFile cache path file


write :: String -> Cache String (String, UTCTime) -> IO ()
write path cache = do
    locked <- L.query(checkFile' path)
    case locked of
        Left err -> putStrLn $ "Error " ++ show err ++ "\n"
        Right lockCheck -> writeIfAvailable lockCheck path cache

writeIfAvailable :: LockCheck -> String -> Cache String (String, UTCTime) -> IO ()
writeIfAvailable check path cache = do
    case (locked check) of
        False -> do
            L.query (lock' path)
            res <- F.query (getFile' path)
            case res of
                Left err -> putStrLn $ "Error: " ++ show err ++ "\n"
                Right file -> do
                    contents <- runUserEditorDWIM plainTemplate (pack $ fileContents file)
                    let updated = File (fileName file) (wrapStr contents)
                    F.query (update updated)
                    D.query (update' updated)
                    L.query (unlock' path)
                    putStrLn "File updated!\n"
                    cacheFile cache path updated

        True -> putStrLn "This file is locked, please try again later when it's lock is released.\n"

newFile :: String -> Cache String (String, UTCTime) -> IO ()
newFile path cache = do
    contents <- runUserEditorDWIM plainTemplate ""
    let file = File path (wrapStr contents)
    L.query (putFile file)
    F.query (putFile' file)
    res <- D.query (put' file)
    cacheFile cache path file
    case res of
        Left err -> putStrLn $ "Error: " ++ show err ++ "\n"
        Right res' -> putStrLn $ show "File added!" ++ "\n"

cacheFile :: Cache String (String, UTCTime) -> String -> File -> IO ()
cacheFile cache path file = do
    time <- getCurrentTime
    insert cache path (fileContents file, time)
