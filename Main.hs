module Main where

import qualified App.Args as A
import App.Parser (applyNTimes)
import qualified App.Parser as P
import qualified App.ToOut as O
import Data.Maybe (fromMaybe)
import qualified Data.Version (showVersion)
import qualified Paths_Atom_counter (version)
import System.Environment (getArgs)
import System.Exit

lobby :: (A.Options, [String]) -> Either (IO ()) String

-- | do actions based on arguments
lobby
  ( A.Options
      { A.optInteractive = isInteractive,
        A.optTable = isTableMode,
        A.optLatex = showLatex,
        A.optShow = showHaskCode,
        A.optVersion = showVersion,
        A.optFold = maxFold,
        A.optToIn = showIn,
        A.optFolds = numOfFolds,
        A.optDebug = isDebug
      },
    xr
    )
    | sum (fromEnum <$> [showHaskCode, showLatex, isTableMode, showIn]) > 1 = Left $ putStrLn "Error: output method mixing"
    | sum (fromEnum <$> [showHaskCode, showLatex, isTableMode, showIn]) == 0 = metaParse $ O.elemsToStr O.toSubscript
    | isInteractive && isTableMode = O.interactiveTable
    | isTableMode = either (Left . putStrLn) Right $ O.tableMode $ filter (/= '\\') $ concat xr
    | showVersion = Right $ "Version: " ++ Data.Version.showVersion Paths_Atom_counter.version
    | showIn = metaParse $ O.elemsToStr O.toInput
    | showHaskCode = metaParse show
    | showLatex = metaParse $ (\a b -> ("(\\" ++) $ (++ "\\)") $ O.elemsToStr a b) O.toLatexSub
    | otherwise = Left $ putStrLn "Error"
    where
      metaParse :: ([P.Element] -> String) -> Either (IO ()) String
      metaParse f
        | isInteractive = O.interactiveMode f parseFormula'
        | otherwise = either (Left . putStrLn) Right $ unwords <$> mapM (fmap f . parseFormula') xr
      parseFormula' :: (String -> Either String [P.Element])
      parseFormula'
        | maxFold = P.foldParseFormula
        | otherwise = flip P.parseFormula numOfFolds

main :: IO ()
main = do
  xIo <- A.compilerOpts <$> getArgs
  xIo >>= (either id putStrLn . lobby)
