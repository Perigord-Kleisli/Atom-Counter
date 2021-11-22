{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -haddock #-}

-- Converts Strings into arrays of Elements
-- > parseFormula "Au_2(Bc_3)_2"
-- =: Right [Leaf ("Au",2)]

module App.Parser where

import Data.Bifunctor (Bifunctor (second))
import Data.Char (chr, isUpper, ord)
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

-- | fst: Name snd: Data
type Element = SyntaxTree String Int

-- | Does not provide error on unbalanced delimeters however
findDelimEnd :: String -> String
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

-- | Get the first valid formula
formHead :: String -> Either String String
formHead [] = Left "Empty formula"
formHead xs
  | null xs && null x = Right []
  | null x = Left $ "Invalid formula: " ++ takeWhile (not . isUpper) xs
  | otherwise = Right x
  where
    x
      | head xs == '(' = findDelimEnd xs
      | otherwise = xs =~ [r|^[A-Z][a-z]?(_[0-9]+)?|] :: String

-- | (The tail is not checked for validity)
formTail :: String -> Either String String
formTail xs
  | null xs && null x = Right []
  | null x = Left $ "Invalid formula: " ++ takeWhile (not . isUpper) xs
  | otherwise = Right $ xs \\ x
  where
    x
      | head xs == '(' = findDelimEnd xs
      | otherwise = xs =~ [r|^[A-Z][a-z]?(_[0-9]+)?|] :: String

-- | Make a list of all the valid formulas
formArray :: String -> [Either String String]
formArray a
  | null xl = map Right $ filter (/= "()") $ init xr
  | otherwise = [Left $ head xl]
  where
    formArray' :: Either String String -> [Either String String]
    formArray' (Left xs) = [Left xs]
    formArray' (Right []) = [Right []]
    formArray' (Right xs) = formHead xs : formArray' (formTail xs)
    (xl, xr) = partitionEithers $ formArray' $ Right a

-- | Takes a single formula and parses it into an Element
formToSTree :: Either String String -> Either String Element
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
formToSTree (Right as) = Right $ Leaf $ second read $ complete $ (\(xl, _ : xr) -> (xl, xr)) $ break (== '_') as
  where
    complete :: (String, String) -> (String, String)
    complete (bl, []) = (bl, "1")
    complete (bl, br) = (bl, br)

-- | Parses the formula and folds it x times
parseFormula :: String -> Int -> Either String [Element]
parseFormula xs n =
  either
    (Left . (++ " in: " ++ xs))
    (Right . applyNTimes n fold1Level)
    $ sequence $ parseFormula' xs
  where
    parseFormula' ys = map formToSTree (formArray $ filter (/= ' ') ys)

-- | Map a node  based on wether its a branch or leaf
leafOrBranch :: (b -> b) -> (b -> b) -> SyntaxTree a b -> SyntaxTree a b
leafOrBranch fl _ (Leaf (xa, xb)) = Leaf (xa, fl xb)
leafOrBranch _ fb (Branch (xa, xb)) = Branch (xa, fb xb)

-- | turn a branch into an array of leaves and branches with a lifting value to their data
liftBranch :: (b -> b -> b) -> SyntaxTree a b -> [SyntaxTree a b]
liftBranch f (Branch (xl, xr)) = map (leafOrBranch (xr `f`) (xr `f`)) xl
liftBranch _ (Leaf x) = [Leaf x]

-- | Lift all branches in a list once
fold1Level :: [Element] -> [Element]
fold1Level = concatMap (liftBranch (*))

-- | Lifts all branches until only leaves remain
minimalFormula :: Either String [Element] -> Either String [Element]
minimalFormula (Left x) = Left x
minimalFormula (Right x) = undefined

-- | Combine elements with the same names
combineSames :: [Element] -> [Element]
combineSames x =
  map (Leaf . foldl (\(_, ar) (bl, br) -> (bl, ar + br)) ("", 0)) $
    groupBy (\(a, _) (b, _) -> a == b) $ map (\(Leaf x) -> x) x

-- | Repeatedly folds a branch until it is comprised only by leaves then combines everything with the same names
untilLeafArray :: Element -> [Element]
untilLeafArray (Leaf x) = [Leaf x]
untilLeafArray xl = concatMap untilLeafArray $ fold1Level [xl]

foldParseFormula :: String -> Either String [Element]
foldParseFormula x =
  filter (\(Leaf (_, xr)) -> xr /= 0) . combineSames . concatMap untilLeafArray
    <$> parseFormula x 1

fromLeaf :: SyntaxTree a b -> Maybe (a, b)
fromLeaf (Leaf x) = Just x
fromLeaf (Branch _) = Nothing

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes 0 _ x = x
applyNTimes n f x = applyNTimes (n - 1) f $ f x
