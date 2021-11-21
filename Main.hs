module Main where

import App.Args (Options (optConsole))
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
        A.optBalancing = isBalanceMode,
        A.optLatex = showLatex,
        A.optConsole = showUnicode,
        A.optShow = showHaskCode,
        A.optVersion = showVersion,
        A.optTable = showTable,
        A.optFold = maxFold,
        A.optFolds = numOfFolds,
        A.optTableArgs = tableArgs,
        A.optDebug = isDebug
      },
    xr
    )
    | sum (fromEnum <$> [showHaskCode, showLatex]) > 1 = Left $ putStrLn "Error: output method mixing"
    | showVersion = Right $ "Version: " ++ Data.Version.showVersion Paths_Atom_counter.version
    | showHaskCode = metaParse show
    | showLatex = metaParse $ (\a b -> ("(\\" ++) $ (++ "\\)") $ O.elemsToStr a b) O.toLatexSub
    | showUnicode = metaParse $ O.elemsToStr O.toSubscript
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
