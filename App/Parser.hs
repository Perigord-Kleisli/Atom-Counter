{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -haddock #-}

-- Converts Strings into arrays of Elements
-- > parseFormula "Au_2(Bc_3)_2"
-- =: Right [Leaf ("Au",2)]

module App.Parser where

import Data.Bifunctor (Bifunctor (second))
import Data.Char (chr, ord)
import Data.Either
  ( fromLeft,
    fromRight,
    isLeft,
    lefts,
    partitionEithers,
    rights,
  )
import Data.Function ()
import Data.List (groupBy, (\\))
import Text.RawString.QQ (r)
import Text.Regex.PCRE (MatchResult (mrBefore), (=~))

data SyntaxTree a b = Leaf (a, b) | Branch ([SyntaxTree a b], b)
  deriving (Show)
-- ^ fst: Name snd: Data

type Element = SyntaxTree String Int

findDelimEnd :: String -> String
-- ^ Does not provide error on unbalanced delimeters however
findDelimEnd [] = []
findDelimEnd (x : xs) = '(' : findDelimEnd' xs
  where
    findDelimEnd' [] = []
    findDelimEnd' (y : ys)
      | y == ')' = y : (ys =~ [r|(_[0-9]+)?|] :: String)
      | y == '(' = y : (findDelimEnd' ys ++ findDelimEnd' postMidDelim)
      | otherwise = y : findDelimEnd' ys
      where
        postMidDelim = ys \\ findDelimEnd' ys

formHead :: String -> Either String String
-- ^ Get the first valid formula
formHead [] = Left "Empty formula"
formHead xs
  | null xs && null x = Right []
  | null x = Left $ "Invalid formula: " ++ xs
  | otherwise = Right x
  where
    x
      | head xs == '(' = findDelimEnd xs
      | otherwise = xs =~ [r|^[A-Z][a-z]?(_[0-9]+)?|] :: String

formTail :: String -> Either String String
-- ^ Get string past first valid formula
-- ^ (The tail is not checked for validity)
formTail xs
  | null xs && null x = Right []
  | null x = Left $ "Invalid formula: " ++ xs
  | otherwise = Right $ xs \\ x
  where
    x
      | head xs == '(' = findDelimEnd xs
      | otherwise = xs =~ [r|^[A-Z][a-z]?(_[0-9]+)?|] :: String

formArray :: String -> [Either String String]
-- ^ Make a list of all the valid formulas
formArray a
  | null xl = map Right $ filter (/= "()") $ init xr
  | otherwise = [Left $ head xl]
  where
    formArray' :: Either String String -> [Either String String]
    formArray' (Left xs) = [Left xs]
    formArray' (Right []) = [Right []]
    formArray' (Right xs) = formHead xs : formArray' (formTail xs)
    (xl, xr) = partitionEithers $ formArray' $ Right a

formToSTree :: Either String String -> Either String Element
-- ^ Takes a single formula and parses it into an Element
formToSTree (Left x) = Left x
formToSTree (Right ('(' : as))
  | isLeft $ head formList = Left $ (head . lefts) formList
  | otherwise =
    (Right . Branch)
      ( rights $ map (formToSTree . Right) $ rights formList,
        complete (as =~ [r|[0-9]+$|] :: String)
      )
  where
    formList = formArray (mrBefore (as =~ [r|(\)(_[0-9]+)?)$|] :: MatchResult String))
    complete :: String -> Int
    complete [] = 1
    complete b = read b :: Int
formToSTree (Right as) = Right $ Leaf $ second read $ complete $ splitOn (== '_') as
  where
    complete :: (String, String) -> (String, String)
    complete (bl, []) = (bl, "1")
    complete (bl, br) = (bl, br)
    splitOn :: Eq a => (a -> Bool) -> [a] -> ([a], [a])
    splitOn f xs = (xl, tail' $ xs \\ xl)
      where
        tail' [] = []
        tail' as = tail as
        xl = takeWhile (not . f) xs

parseFormula :: String -> Either String [Element]
-- ^ Parses the formula but does not fold it
parseFormula xs =
  if isLeft $ head x
    then Left $ fromLeft [] $ head x
    else Right $ rights x
  where
    x = parseFormula' xs
    parseFormula' ys = map formToSTree (formArray $ filter (/= ' ') ys)

leafOrBranch :: (b -> b) -> (b -> b) -> SyntaxTree a b -> SyntaxTree a b
-- ^ Map a node  based on wether its a branch or leaf
leafOrBranch fl _ (Leaf (xa, xb)) = Leaf (xa, fl xb)
leafOrBranch _ fb (Branch (xa, xb)) = Branch (xa, fb xb)

liftBranch :: (b -> b -> b) -> SyntaxTree a b -> [SyntaxTree a b]
-- ^ turn a branch into an array of leaves and branches with a lifting value to their data
liftBranch f (Branch (xl, xr)) = map (leafOrBranch (xr `f`) (xr `f`)) xl
liftBranch _ (Leaf x) = [Leaf x]

fold1Level :: [Element] -> [Element]
-- ^ Lift all branches in a list once
fold1Level = concatMap (liftBranch (*))

minimalFormula :: Either String [Element] -> Either String [Element]
-- ^ Lifts all branches until only leaves remain
minimalFormula (Left x) = Left x
minimalFormula (Right x) = undefined

combineSames :: [Element] -> [Element]
-- ^ Combine elements with the same names
combineSames x =
  map (Leaf . foldl (\(_, ar) (bl, br) -> (bl, ar + br)) ("", 0)) $
    groupBy (\(a, _) (b, _) -> a == b) $ map (\(Leaf x) -> x) x

untilLeafArray :: Element -> [Element]
-- ^ Repeatedly folds a branch until it is comprised only by leaves then combines everything with the same names
untilLeafArray (Leaf x) = [Leaf x]
untilLeafArray xl = concatMap untilLeafArray $ fold1Level [xl]

foldParseFormula :: String -> Either String [Element]
foldParseFormula x =
  if isLeft xs
    then xs
    else Right $ filter (\(Leaf (_, xr)) -> xr /= 0) $ combineSames $ concatMap untilLeafArray $ fromRight [] xs
  where
    xs = parseFormula x

toSubscript :: Int -> String
toSubscript = map (chr . (+ 8272) . ord) . show

elemsToStr :: [Element] -> String
elemsToStr = filter (/= '₁') . concatMap fromElem
  where
    fromElem :: Element -> String
    fromElem (Leaf (al, ar)) = al ++ toSubscript ar
    fromElem (Branch (al, ar)) = "(" ++ concatMap fromElem al ++ ")" ++ toSubscript ar
