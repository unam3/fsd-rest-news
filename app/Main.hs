module Main where

import System.Environment (getArgs)

import RestNews (runWarpWithLogger)

main :: IO ()
main = getArgs >>= runWarpWithLogger
