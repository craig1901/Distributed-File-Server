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
                ["get", filePath] -> do
                    putStrLn $ "getting " ++ filePath ++ "...\n"
                    getFile filePath
                    input
                ["write", file] -> do
                    newFile file
                    input
                ["quit"] -> do
                    putStrLn "Bye!"
                    return ()

                _ -> do
                    putStrLn $ "Wrong!\n" ++ "usage: get <filePath>\n" ++ "write <filePath>" 
                    input
