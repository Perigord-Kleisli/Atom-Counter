{-# LANGUAGE QuasiQuotes #-}

-- For making strings of element datatypes

module App.ToOut where

import App.Args
import App.Parser (Element, SyntaxTree (Branch, Leaf), combineSames, foldParseFormula, formArray, fromLeaf, parseFormula)
import Control.Monad.Cont
import Data.Bifunctor
import Data.Char (chr, isAlphaNum, isDigit, isNumber, isSpace, isUpper, ord)
import Data.Either
import Data.Function
import Data.Functor (($>), (<&>))
import Data.List
import Data.Maybe (fromMaybe)
import Text.RawString.QQ
import Text.Regex.PCRE

toSubscript :: Int -> String
toSubscript = map (chr . (+ 8272) . ord) . show

toLatexSub :: Int -> String
toLatexSub x = "_{" ++ show x ++ "}"

toInput :: Int -> String
toInput = ("_" ++) . show

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

interactiveTable :: Either (IO ()) String
interactiveTable = Left interactLoop
  where
    interactLoop = do
      putStrLn "Atom-Counter: Interactive mode"
      withBreak $ \break ->
        forM_ [1 ..] $ \i -> do
          a <- liftIO getLine
          when (a == ":q") $ break ()
          liftIO . either putStrLn putStrLn $ tableMode a
      liftIO . putStrLn $ "Interactive Mode End"
      where
        withBreak = flip runContT pure . callCC

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

-- | Adds spaces to both sides of a string until it reaches the target width
respaceList :: Int -> a -> [a] -> [a]
respaceList n x xs
  | length xs > n = reverse $ (drop l . reverse) $ drop r xs
  | otherwise = replicate l x ++ xs ++ replicate r x
  where
    (l, r) = (\x -> (floor x, ceiling x)) (fromIntegral (n - length xs) / 2)

respaceString :: Int -> String -> String
respaceString 0 xs = xs
respaceString n xs = replicate l ' ' ++ xs ++ replicate r ' '
  where
    (l, r) = (\x -> (floor x, ceiling x)) (fromIntegral (n - length xs) / 2)

tableMode :: String -> Either String String
tableMode xs = stringTable
  where
    -- This became more verbose than what I imagined
    stringTable = divider >>= \div -> spacedTable <&> (div ++) . (++ div) . intercalate div -- The table as a single String
    surround x = "|" ++ x ++ "|\n" -- Adds the outer borders of the table
    divider = surround . intercalate "+" . map (`replicate` '-') <$> sequence maxWidths -- Adds the horizontal dividers of the table
    spacedTable =
      -- Equalizes collumn width and adds borders cellwise
      sequence maxWidths >>= \mw ->
        table
          <&> map (surround . intercalate "|" . zipWith respaceString mw)
    maxWidths =
      -- The highest width cells of every collumn
      let maxVal n = length . maximumBy (compare `on` length) . map (!! n) <$> table
       in [maxVal 0, maxVal 1, maxVal 2]
    table = (["", " Reactant ", " Product "] :) . map (map (\x -> " " ++ x ++ " ")) <$> xy -- Places all the elements in a 2d list
    xy = map (\[Leaf (x, y), Leaf (_, y2)] -> [x, show y, show y2]) <$> xyGrouped
      where
        xyGrouped =
          either Left (\y -> if all ((>= 2) . length) y then Right y else Left $ "Elements unique to one collumn " ++ show y) $
            groupBy ((==) `on` leafName)
              . sortBy (compare `on` leafName)
              . concat
              <$> sequence [xl, xr]

    leafName (Leaf (y, _)) = y -- simple helper function for getting the element name
    leafName y = error $ "Not a leaf " ++ show y

    -- Elements seperated by the middle arrow
    (xl, xr) = either (\x -> (Left x, Left x)) id xlxr
    xlxr =
      let xls = replaceBlanks xs <&> break (or . zipWith (==) possibleDividers . repeat) . words
          f = fmap (combineSames . concat) . mapM foldParseFormula' . init . flip tupListBy (all isNumber)
       in bimap f (either Left f . tail') <$> xls
      where
        -- in (f xl', either Left f $ tail' xr')

        possibleDividers = ["->", "→", "\\rightarrow"]
        tail' [] = Left "No divider"
        tail' x = Right $ tail x

    -- multiplies all the elements by a preceeding multiplier
    foldParseFormula' (yl, yr) =
      fmap (map (\(Leaf z) -> Leaf $ second (* (read (if null yl then "1" else yl) :: Int)) z) . concat)
        <$> mapM foldParseFormula
        $ filter (all (\x -> isAlphaNum x || x == '_')) yr

-- partedVals@[blanks, digits, xs'] = partitionOn [(=~ [r|^_+.*|]), all isDigit, (True `const`)] $ words xs

-- | "_Au + _Cd -> Ln 2 3" =: "2 Au + 3 Cd -> Ln"
replaceBlanks :: String -> Either String String
replaceBlanks xs =
  if ((==) `on` length) blanks digits
    then Right $ unwords $ take (length xs' - length digits) $ concatIfEqual unblanks xs'
    else Left "Coeffients and blanks are not equal in count"
  where
    concatIfEqual [] [] = [[]]
    concatIfEqual [] y2 = y2
    concatIfEqual _ [] = [[]]
    concatIfEqual ylr@((y1l, y1r) : y1s) (y2 : y2s) =
      if y1r == y2
        then (y1l ++ " " ++ dropWhile (== '_') y1r) : concatIfEqual y1s y2s
        else y2 : concatIfEqual ylr y2s
    unblanks = zip digits blanks
    partedVals@[blanks, digits, xs'] = partitionOn [(=~ [r|^\\?_+.*|]), all isDigit, (True `const`)] $ words xs

-- | Partitions a list where the level is based on whichever argument from the function list it becomes true in,
--
-- > ParitionOn [(>1), (==1), (<1)] [2,3,1,2,-1,-4,2] == [[-1,-4],[1],[2,3,2,2]]
partitionOn :: [a -> Bool] -> [a] -> [[a]]
partitionOn [] _ = [[]]
partitionOn [f] xs = [filter f xs]
partitionOn (f : fs) xs = filter f xs : partitionOn fs xs

insertAt :: a -> Int -> [a] -> [a]
insertAt a i xs = xl ++ xr
  where
    xr = a : xr'
    (xl, xr') = splitAt i xs

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither x Nothing = Left x
maybeToEither _ (Just x) = Right x

tupListBy :: [[a]] -> ([a] -> Bool) -> [([a], [[a]])]
tupListBy [] _ = [([], [])]
tupListBy (x : xs) f = ([], xl) : tupListBy' xr f
  where
    (xl, xr) = break f (x : xs)
    tupListBy' [] _ = [([], [])]
    tupListBy' (y : ys) f2
      | f2 y = let (yl, yr) = break f2 ys in (y, yl) : tupListBy' yr f2
      | otherwise = tupListBy ys f2

maybeList :: [a] -> Maybe [a]
maybeList [] = Nothing
maybeList xs = Just xs

-- | Breaks an array into subarrys
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn f s = case dropWhile f s of
  [] -> []
  s' -> w : splitOn f s''
    where
      (w, s'') =
        break f s'
