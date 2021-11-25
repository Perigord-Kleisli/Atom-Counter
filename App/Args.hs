-- Parses program arguments and controls what actions are done
{-# LANGUAGE QuasiQuotes #-}

module App.Args where

import Data.Maybe
import System.Console.GetOpt
import Text.RawString.QQ

data Options = Options
  { optInteractive :: Bool,
    optHelpPage :: Bool,
    optTable :: Bool,
    optLatex :: Bool,
    optShow :: Bool,
    optVersion :: Bool,
    optFold :: Bool,
    optFolds :: Int,
    optDebug :: Bool
  }
  deriving (Show)

defaultOptions =
  Options
    { optInteractive = False,
      optHelpPage = False,
      optTable = False,
      optLatex = False,
      optShow = False,
      optVersion = False,
      optFold = False,
      optFolds = 0,
      optDebug = False
    }

options :: [OptDescr (Options -> Options)]
options =
  [ Option
      ['i', 'I']
      ["interactive"]
      (NoArg (\opts -> opts {optInteractive = True}))
      "Use STDIN instead of console arguments, type :q to quit",
    Option
      ['t', 'T']
      ["table"]
      (NoArg (\opts -> opts {optTable = True}))
      "Unsubscripted Numbers gets treated as multipliers, arrays of arguments also get accepted with symbols other than parenthesis acting as delimeters",
    Option
      ['l', 'L']
      ["latex"]
      (NoArg (\opts -> opts {optLatex = True}))
      "Send output as a latex formula",
    Option
      ['v', 'V']
      ["version"]
      (NoArg (\opts -> opts {optVersion = True}))
      "Show program version",
    Option
      ['s', 'S']
      ["show"]
      (NoArg (\opts -> opts {optShow = True}))
      "Show output via actual value in haskell",
    Option
      ['f']
      ["fold"]
      (NoArg (\opts -> opts {optFold = True}))
      "Repeatedly fold formula until at lowest term",
    Option
      ['F']
      ["folds"]
      ( OptArg
          ((\f opts -> opts {optFolds = read f :: Int}) . fromMaybe "0")
          "NUMBER"
      )
      "Fold formula a number of times",
    Option
      ['D', 'd']
      ["debug"]
      (NoArg (\opts -> opts {optDebug = True}))
      "show debug information"
  ]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: [OPTION...] formula..."
