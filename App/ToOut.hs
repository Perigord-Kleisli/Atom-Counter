-- For unicode output
module App.ToOut where

import App.Args
import App.Parser (Element, SyntaxTree (Branch, Leaf), combineSames, foldParseFormula, formArray, fromLeaf, parseFormula)
import Control.Monad.Cont
import Data.Char (chr, isAlphaNum, isUpper, ord)
import Data.Either
import Data.Function
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
    --fromElem (Leaf (_, 0)) = ""
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

respaceString :: Int -> String -> String
respaceString 0 xs = xs
respaceString n xs = replicate l ' ' ++ xs ++ replicate r ' '
  where
    (l, r) = (\x -> (floor x, ceiling x)) (fromIntegral (n - length xs) / 2)

balanceMode :: (Int -> String) -> String -> Either (IO ()) String
balanceMode iToS xs = either (Left . putStrLn) Right stringTable
  where
    stringTable = (++ fromRight [] divider) . (fromRight [] divider ++) . intercalate (fromRight [] divider) <$> spacedTable
    surround x = "|" ++ x ++ "|\n"
    divider = surround . intercalate "+" . map (`replicate` '-') <$> sequence maxWidths
    spacedTable = map (surround . intercalate "|" . zipWith respaceString (fromRight [] $ sequence maxWidths)) <$> table
    maxWidths =
      let maxVal n = length . maximumBy (compare `on` length) . map (!! n) <$> table
       in [maxVal 0, maxVal 1, maxVal 2]
    table = (["", " Reactant ", " Product "] :) . map (map (\x -> " " ++ x ++ " ")) <$> xy
    xy =
      map (\[Leaf (x, y), Leaf (x2, y2)] -> [x, iToS y, iToS y2])
        . groupBy ((==) `on` leafName)
        . sortBy (compare `on` leafName)
        . concat
        <$> sequence [xl, xr]
    leafName (Leaf (y, _)) = y
    leafName y = error $ "Not a leaf " ++ show y
    xl = combineSames . concat <$> mapM foldParseFormula (filter (/= "+") xl')
    xr = combineSames . concat <$> mapM foldParseFormula (filter (/= "+") xr')
    (xl', _ : xr') = break (\x -> x == "->" || x == "\\rightarrow") $ words xs

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn f s = case dropWhile f s of
  [] -> []
  s' -> w : splitOn f s''
    where
      (w, s'') =
        break f s'

toTable :: [Element] -> String
toTable = undefined
