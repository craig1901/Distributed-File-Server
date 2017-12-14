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
                    listFiles
                    input cache
                ["get", filePath] -> do
                    getFile filePath cache
                    input cache
                ["write", filePath] -> do
                    write filePath cache
                    input cache
                ["new", file] -> do
                    newFile file cache
                    input cache
                ["quit"] -> do
                    putStrLn "Bye!"
                    return ()
                _ -> do
                    putStrLn $ "Wrong!\n" ++ "usage:ls\nget <filePath>\nnew <filePath>\nwrite <filePath>\n\n"
                    input cache
