{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Parser where

import           Control.Applicative
import           Data.Char
import           Data.Foldable
import           Data.Function
import           Data.Functor
import           Data.Maybe
import           Data.Tuple


newtype Parser a = Parser {
  runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser parser) = Parser ((fmap . fmap) f . parser)

instance Applicative Parser where
  pure x = Parser $ Just . (, x)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (input_1, f) <- p1 input
    (input_2, a) <- p2 input_1
    pure (input_2, f a)

instance Alternative Parser where
  empty = Parser $ const Nothing

  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

data Element = Elements ([Element], Int) | Elem (String,Int)
  deriving (Show, Eq)

isElem :: Element -> Bool
isElem (Elem _) = True
isElem _        = False

instance Ord Element where
  compare (Elements _     ) (Elements _     ) = LT
  compare (Elem     _     ) (Elements _     ) = GT
  compare (Elements _     ) (Elem     _     ) = GT
  compare (Elem     (x, _)) (Elem     (y, _)) = compare x y

boolP :: (Char -> Bool) -> Parser Char
boolP f = Parser
  (\case
    y : ys | f y -> Just (ys, y)
    _            -> Nothing
  )

charP :: Char -> Parser Char
charP c = boolP (== c)

stringP :: String -> Parser String
stringP = traverse charP

whiteSpace :: Parser String
whiteSpace = many (boolP isSpace)

elemNameP :: Parser String
elemNameP = Parser $ \input -> do
  (residue, x) <- runParser (boolP isUpper) input
  case runParser (boolP isLower) residue of
    Nothing            -> return (residue, [x])
    Just (residue', y) -> return (residue', [x, y])

numP :: Parser Int
numP = read . toNumber <$> some (boolP isNumber)
 where
  toNumber = map (\x -> if isSubNum x then unSubNum x else x)
  isSubNum = (`elem` "₀₁₂₃₄₅₆₇₈₉")
  unSubNum n = toEnum @Char $ fromEnum n - 8272

elemNumP :: Parser Int
elemNumP =
  charP '_'
    *>  numP
    <|> read
    .   map unSubNum
    <$> some (boolP isSubNum)
    <|> (charP '{' *> numP <* charP '}')
    <|> pure 1
 where
  isSubNum = (`elem` "₀₁₂₃₄₅₆₇₈₉")
  unSubNum n = toEnum @Char $ fromEnum n - 8272

elemP :: Parser Element
elemP = (Elem .) . (,) <$> elemNameP <*> elemNumP

elemBracketP :: Parser Element
elemBracketP =
  (Elements .)
    .   (,)
    <$> (  charP '('
        *> whiteSpace
        *> some (elemP <|> elemBracketP)
        <* whiteSpace
        <* charP ')'
        )
    <*> elemNumP

sepByParser :: Parser b -> Parser a -> Parser [b]
sepByParser element sep  = (:) <$> element <*> many (sep *> element)
                          <|> pure []

formulaP :: Parser [Element]
formulaP = many $ whiteSpace *> (elemP <|> elemBracketP) <* whiteSpace

