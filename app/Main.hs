module Main where

import Lib hiding (someFunc)
import File

main :: IO ()
main = someFunc
