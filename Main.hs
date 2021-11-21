module Main where

import App.Args (Options (optConsole))
import qualified App.Args as A
import qualified App.Parser as P
import qualified App.ToOut as O
import qualified Data.Version (showVersion)
import qualified Paths_Atom_counter (version)
import System.Environment (getArgs)
import System.Exit

lobby :: (A.Options, [String]) -> Either String String

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
    | showVersion = Right $ "Version: " ++ Data.Version.showVersion Paths_Atom_counter.version
    | showHaskCode = metaParse show
    | showLatex = metaParse $ O.elemsToStr O.toLatexSub
    | showUnicode = metaParse $ O.elemsToStr O.toSubscript
    where
      metaParse :: ([P.Element] -> String) -> Either String String
      metaParse f = unwords <$> mapM (fmap f . parseFormula' numOfFolds maxFold) xr
        where
          parseFormula' :: Int -> Bool -> (String -> Either String [P.Element])
          parseFormula' x False = either Left (applyNtimes) P.parseFormula

main :: IO ()
main = do
  xIo <- A.compilerOpts <$> getArgs
  xIo >>= (either putStrLn putStrLn . lobby)
