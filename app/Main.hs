module Main where

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



mainLoop :: FSZipper -> IO ()
mainLoop z = do
    putStr "$ "
    cmd <- getLine
    let result = case cmd of
                            "ls" -> ls z
                            ('c':'d':xs) -> cd xs
                            "exit" -> fail
        resText = case result of
                    Left e -> show e
                    Right (_, t) -> t
        newz = case result of
                  Left _ -> z
                  Right (nz, _) -> nz
    putStr result
    mainLoop newz


main :: IO ()
main = do
    putStrLn $ "Practical FileSystem: v" ++ (show version) ++ "."
    putStrLn "Made by Cj-bc"
    let diskZipper = (sampleDisk, [])
    mainLoop diskZipper
