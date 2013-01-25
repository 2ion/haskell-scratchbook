module Main
    where

import Data.Maybe

main = do
    putStrLn "Please enter your name: "
    name <- getLine
    putStrLn ("Hello " ++   name)
