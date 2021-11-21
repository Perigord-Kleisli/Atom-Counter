-- For unicode output
module App.ToOut where

import App.Args
import App.Parser (Element, SyntaxTree (Branch, Leaf), foldParseFormula, formArray, parseFormula)
import Control.Monad.Cont
import Data.Char (chr, isUpper, ord)
import Data.List
import Text.Regex.PCRE

toSubscript :: Int -> String
toSubscript = map (chr . (+ 8272) . ord) . show

toLatexSub :: Int -> String
toLatexSub x = "_{" ++ show x ++ "}"

elemsToStr :: (Int -> String) -> [Element] -> String
elemsToStr f = concatMap fromElem
  where
    filterTrailingOne :: String -> String
    filterTrailingOne a =
      if a =~ "_1$"
        then reverse $ drop 2 $ reverse a
        else a
    fromElem :: Element -> String
    fromElem (Leaf (al, ar)) = al ++ (if ar == 1 then [] else f ar)
    fromElem (Branch (al, ar)) = "(" ++ concatMap fromElem al ++ ")" ++ f ar

interactiveMode ::
  ([Element] -> String) ->
  (String -> Either String [Element]) ->
  Either (IO ()) String
interactiveMode f fs = Left $ interactLoop f fs

interactLoop :: ([Element] -> String) -> (String -> Either String [Element]) -> IO ()
interactLoop x y = do
  putStrLn "Atom-Counter: Interactive mode"
  withBreak $ \break ->
    forM_ [1 ..] $ \i -> do
      a <- liftIO getLine
      when (a == ":q") $ break ()
      liftIO . putStrLn $ either id x (y a)
  liftIO . putStrLn $ "Interactive Mode End"
  where
    withBreak = flip runContT pure . callCC
