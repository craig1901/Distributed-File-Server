module Main where

import Client

main :: IO ()
main = do
    putStrLn "Welcome to Craig's File Server!\n"
    input
    where
        input = do
            command <- getLine
            case words command of
                ["ls"] -> do
                    putStrLn ""
                    listFiles
                    input
                ["get", filePath] -> do
                    getFile filePath
                    input
                ["write", filePath] -> do
                    write filePath
                    input
                ["new", file] -> do
                    newFile file
                    input
                ["quit"] -> do
                    putStrLn "Bye!"
                    return ()
                _ -> do
                    putStrLn $ "Wrong!\n" ++ "usage: get <filePath>\n" ++ "new <filePath>\n\n"
                    input
