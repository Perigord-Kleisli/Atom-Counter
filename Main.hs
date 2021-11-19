module Main where

import qualified App.Args as A
import qualified App.Parser as P
import qualified App.ToOut as O
import System.Environment (getArgs)
import System.Exit

lobby :: (A.Options, [String]) -> Either String String

-- | do actions based on arguments
lobby
  ( A.Options
      { A.optInteractive = isInteractive,
        A.optHelpPage = showHelp,
        A.optBalancing = isBalanceMode,
        A.optLatex = showLatex,
        A.optChemfig = showChemLatex,
        A.optConsole = showUnicode,
        A.optShow = showHaskCode,
        A.optVersion = showVersion,
        A.optTable = showTable,
        A.optFold = maxFold,
        A.optFolds = numOfFods,
        A.optTableArgs = tableArgs,
        A.optDebug = isDebug
      },
    xr
    )
    | otherwise = metaParse O.elemsToStr
    where
      metaParse :: ([P.Element] -> String) -> Either String String
      metaParse f = unwords <$> mapM (fmap f . P.parseFormula) xr

main :: IO ()
main = do
  xIo <- A.compilerOpts <$> getArgs
  xIo >>= (either putStrLn putStrLn . lobby)
