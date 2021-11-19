-- For unicode output
module App.ToOut where

import App.Parser (Element, SyntaxTree (Branch, Leaf))
import Data.Char (chr, ord)

toSubscript :: Int -> String
toSubscript = map (chr . (+ 8272) . ord) . show

elemsToStr :: [Element] -> String
elemsToStr = filter (/= '₁') . concatMap fromElem
  where
    fromElem :: Element -> String
    fromElem (Leaf (al, ar)) = al ++ toSubscript ar
    fromElem (Branch (al, ar)) = "(" ++ concatMap fromElem al ++ ")" ++ toSubscript ar
