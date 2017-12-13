module Main where

import Client
import Data.Cache
import Data.Time

main :: IO ()
main = do
    putStrLn "Welcome to Craig's File Server!\n"
    cache <- newCache Nothing :: IO (Cache String (String, UTCTime))
    input cache
    where
        input cache = do
            command <- getLine
            case words command of
                ["ls"] -> do
                    putStrLn ""
                    listFiles cache
                    input cache
                ["get", filePath] -> do
                    getFile filePath cache
                    input cache
                ["write", filePath] -> do
                    write filePath
                    input cache
                ["new", file] -> do
                    newFile file
                    input cache
                ["quit"] -> do
                    putStrLn "Bye!"
                    return ()
                _ -> do
                    putStrLn $ "Wrong!\n" ++ "usage: get <filePath>\n" ++ "new <filePath>\n\n"
                    input cache
