module Main where

import Control.Monad
import FileSystem
import FileSystem.Commands

version :: (Int, Int, Int)
version = (0,1,0)

sampleDisk :: FSItem
sampleDisk = Directory "root"
                [ Directory "bin"
                    [ File "bash" "BASH EXECUTABLE"
                    , File "vi" "VI EXECUTABLE"
                    , File "vim" "VIM EXECUTABLE"
                    ]
                , Directory "config"
                    [ File "bashrc" "bashrc CONTENTS"
                    ]
                , Directory "Pic"
                    [ File "Yuaaaa.png" "Datentch Yua's photo"
                    , File "Yozakura_mia_bear.png" "Yozakura Mia(bear)'s photo"
                    ]
                ]

commandMap :: String -> FSZipper -> Either FSError (FSZipper, String)
commandMap "ls"             z = ls z
commandMap ('c':'d':' ':xs) z = cd xs z
commandMap "exit"           _ = Left ExitCalled
commandMap "zipper"         z = Right (z, show z)
commandMap ""               z = Right (z, "")
commandMap _                _ = Left CommandNotFound


mainLoop :: FSZipper -> IO ()
mainLoop z = do
    cmd <- getLine
    let result = commandMap cmd z
        resText = case result of
                    Left e -> show e
                    Right (_, t) -> t
        newz = case result of
                  Left _ -> z
                  Right (nz, _) -> nz

    when (resText /= "") $ putStrLn resText
    when (resText /= show ExitCalled) $ mainLoop newz


main :: IO ()
main = do
    putStrLn $ "Practical FileSystem: v" ++ (show version) ++ "."
    putStrLn "Made by Cj-bc"
    let diskZipper = (sampleDisk, [])
    mainLoop diskZipper
