module Main where

import Lib

main :: IO ()
main = normalizeDB >> makeBackups
