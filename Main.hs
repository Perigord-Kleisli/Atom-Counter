module Main where

import qualified App.Args as A
import qualified App.Parser as P

main :: IO ()
main = do
  x <- getLine
  print $ P.foldParseFormula x
