module ToString where

import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor
import           Data.Char
import           Data.Function
import           Data.List
import           Parser
import           Process
import           Text.Printf



toString :: (Int -> String) -> Element -> String
toString f (Elem     (n , i)) = n ++ if i == 1 then "" else f i
toString f (Elements (xs, i)) = "(" ++ concatMap (toString f) xs ++ ")" ++ if i == 1 then "" else f i

toSub, toBasic, toLatex :: Int -> String
toSub = map (chr . (+ 8272) . ord) . show
toBasic = ("_" ++) . show
toLatex = printf "_{%s}" . show

elemsToStr :: (Int -> String) -> [Element] -> String
elemsToStr f = concatMap (toString f)

respaceStr :: Int -> String -> String
respaceStr 0 xs = xs
respaceStr n xs = replicate l ' ' ++ xs ++ replicate r ' '
 where
  (l, r) = (\x -> (floor x, ceiling x)) (fromIntegral (n - length xs) / 2)

orgTable :: ([Element], [Element]) -> String
orgTable table =
  unlines
    $ ([head separators, "|    | Reactant |    | Product |",head separators]++)
    $ (++ separators)
    $ intercalate separators
    $ map (: [])
    $ uncurry (zipWith (printf "|%s|%s|"))
    $ bimap (map (separate True)) (map (separate False)) table
 where
  separators = ["|----+----------+----+---------|"]
    -- 3 is the largest element name size + margin 
    -- 9,10 is the size of Reactant,Product + margin
  separate :: Bool -> Element -> String
  separate isLeft (Elem x) = uncurry (printf "%s|%s")
    $ bimap (respaceStr 4) (respaceStr (if isLeft then 10 else 9) . show) x
  separate isLeft (Elements x) = uncurry (printf "|%s|%s|") $ bimap
    (respaceStr 3 . concatMap (toString show))
    (respaceStr (if isLeft then 10 else 9) . show)
    x
