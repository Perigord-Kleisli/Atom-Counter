{-# LANGUAGE QuasiQuotes #-}

-- For making strings of element datatypes

module App.ToOut where

import App.Args
import App.Parser (Element, SyntaxTree (Branch, Leaf), combineSames, foldParseFormula, formArray, fromLeaf, parseFormula)
import Control.Monad.Cont
import Data.Bifunctor
import Data.Char (chr, isAlphaNum, isNumber, isSpace, isUpper, ord)
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
          liftIO . either putStrLn putStrLn $ tableMode show a
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

tableMode :: (Int -> String) -> String -> Either String String
tableMode iToS xs = stringTable
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
    xy = map (\[Leaf (x, y), Leaf (_, y2)] -> [x, iToS y, iToS y2]) <$> xyGrouped
      where
        xyGrouped =
          either Left (\y -> if all ((>= 2) . length) y then Right y else Left $ "Elements unique to one collumn" ++ show y) $
            groupBy ((==) `on` leafName)
              . sortBy (compare `on` leafName)
              . concat
              <$> sequence [xl, xr]
    leafName (Leaf (y, _)) = y -- simple helper function for getting the element name
    leafName y = error $ "Not a leaf " ++ show y
    (xl, xr) =
      -- Elements seperated by the middle arrow
      let (xl', xr') = break (\x -> x == "->" || x == "\\rightarrow" || x == "→") $ words xs
          f = fmap (combineSames . concat) . mapM foldParseFormula' . init . flip tupListBy (all isNumber)
       in (f xl', either Left f $ tail' xr')
      where
        tail' [] = Left "No divider"
        tail' x = Right $ tail x
    foldParseFormula' (yl, yr) =
      -- multiplies all the elements by a preceeding multiplier
      fmap (map (\(Leaf z) -> Leaf $ second (* (read (if null yl then "1" else yl) :: Int)) z) . concat)
        <$> mapM foldParseFormula
        $ filter (all (\x -> isAlphaNum x || x == '_')) yr

replaceBlanks :: String -> Either String String
-- replaceBlanks xs = xlr >>= \xlr' -> Right $ if null xlr' then xs else unwords xlr'
replaceBlanks xs = Left $ show xlxr
  where
    xlr =
      lastDigits >>= \lastDigits' ->
        firstWords >>= \firstWords' ->
          if ((==) `on` length) lastDigits' firstWords'
            then Right $ zipWith (\y0' y1' -> " " ++ y0' ++ " " ++ y1' ++ "") lastDigits' firstWords'
            else Left "Number of Elements and digits is imbalanced"

    lastDigits = (\div' divI' -> insertAt div' divI' $ words (xs =~ [r|\s(\d+\s)*\d$|])) <$> divider <*> divIndex
    divIndex = divider >>= \div' -> maybeToEither "No divider" $ findIndex (=~ div') firstWords'
    divider =
      let ys = (getAllTextMatches $ xs =~ [r|(->|\rightarrow|→)|] :: [String])
       in if length ys > 1 then Left "More than one divider" else head' ys

    xlxr = (\a b c -> join bimap ((map . map) (fromMaybe " ")) $ anchorOn a [b, c]) <$> divider <*> firstWords <*> lastDigits

    head' [] = Left "No divider"
    head' x = Right $ head x

    firstWords =
      divider >>= \div' ->
        return $
          map (dropWhile (\y -> isSpace y || y == '_')) $
            filter (\y -> y == div' || y =~ [r|\s*_+.+|]) firstWords'
    firstWords' = getAllTextMatches $ xs =~ [r|(->|\rightarrow|→|(_+)?[A-Z][a-z]?(_\d+)?)|] :: [String]

respaceLeft y n ys = let n' = n - length ys in replicate n' y ++ ys

respaceRight y n ys = let n' = n - length ys in ys ++ replicate n' y

-- | Creates a tuple of 2d lists where the start of snd will be based on an 'anchor',
-- The blank space are 'Nothings' you are to treat as Null in whatever datatype you so choose
--  >>> join bimap ((map . map) (fromMaybe ' ')) $ anchorOn \'a\' ["hat", "sfad", "sfdhad", "asddd"]
--  (["   h","  sf","sfdh","    "],["at   ","ad   ","ad   ","asddd"])
anchorOn :: (Eq a) => a -> [[a]] -> ([[Maybe a]], [[Maybe a]])
anchorOn anchor xs = xs''
  where
    xs'' = bimap (map (respaceLeft Nothing maxLeft)) (map (respaceRight Nothing maxRight)) xs'
    maxRight = length $ maximumBy (compare `on` length) $ snd xs'
    maxLeft = length $ maximumBy (compare `on` length) $ fst xs'
    xs' = unzip $ map (break (== anchor') . map Just) xs
    anchor' = Just anchor

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
