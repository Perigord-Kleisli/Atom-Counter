-- For unicode output
module App.ToOut where

import App.Parser (Element, SyntaxTree (Branch, Leaf))
import Data.Char (chr, ord)

toSubscript :: Int -> String
toSubscript = map (chr . (+ 8272) . ord) . show

toLatexSub :: Int -> String
toLatexSub x = "_{" ++ show x ++ "}"

elemsToStr :: (Int -> String) -> [Element] -> String
elemsToStr f = filter (/= '₁') . concatMap fromElem
  where
    fromElem :: Element -> String
    fromElem (Leaf (al, ar)) = al ++ f ar
    fromElem (Branch (al, ar)) = "(" ++ concatMap fromElem al ++ ")" ++ f ar
