module Process where

import Parser
import Data.Bifunctor
import Data.List
import Data.Function
import Control.Applicative
import Args 


onLeaforBranch :: 
     ((String,Int) -> (String,Int))
  -> (([Element],Int)-> ([Element], Int))
  -> Element
  -> Element

onLeaforBranch f _ (Elem form)  = Elem $ f form
onLeaforBranch  _ f (Elements forms)  = Elements $ f forms

foldForm :: Element -> [Element]
foldForm (Elem form) = [Elem form]
foldForm (Elements (elems,n)) = map (onLeaforBranch (second (*n)) (second (*n))) elems

groupForm :: [Element] -> [Element]
groupForm [] = []
groupForm xs = map foldFun $ group $ sort xs
  where
    foldFun [] = undefined
    foldFun ((Elem x) : xs) = foldr (\(Elem (n,i)) (Elem (_,j)) -> Elem (n, i+j)) (Elem x) xs
    foldFun ((Elements x) : _) = Elements $ first groupForm x

applyUntil :: (a -> Bool) -> (a -> a) -> a -> a
applyUntil f_bool f_x x = if f_bool x
  then x
  else applyUntil f_bool f_x (f_x x)

flattenForm :: [Element] -> [Element]
flattenForm xs = applyUntil (all isElem) (concatMap foldForm) $ groupForm xs

reactProdP :: Parser ([Element], [Element])
reactProdP =
  ((,) `on` (flattenForm . concat))
    <$> formulaP `sepByParser` charP '+'
    <*> (whiteSpace *> separators 
        *> formulaP `sepByParser` charP '+')
 where
  separators = foldr1 (<|>) [ stringP x | x <- ["->", "→", "\\rightarrow"] ]


