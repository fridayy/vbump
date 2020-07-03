module Main where

import Vbump
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    contents <- getContents
    let arg = (parseArgs args)
    putStr (bump arg $ latestVersion $ lines contents)