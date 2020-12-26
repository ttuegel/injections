module Main (main) where

import Injection (projectName)


main :: IO ()
main = putStrLn ("Tests for " ++ projectName)
