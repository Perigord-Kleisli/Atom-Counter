module Main where

import System.Console.GetOpt
import System.Environment

import Interactive
import Args


main :: IO ()
main = do
  args <- compilerOpts <$> getArgs
  args >>= (either (putStrLn =<<)putStrLn . lobby)
