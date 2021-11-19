-- Parses program arguments and controls what actions are done
{-# LANGUAGE QuasiQuotes #-}

module App.Args where

import Data.Maybe
import System.Console.GetOpt
import Text.RawString.QQ

data Options = Options
  { optInteractive :: Bool,
    optHelpPage :: Bool,
    optBalancing :: Bool,
    optLatex :: Bool,
    optChemfig :: Bool,
    optConsole :: Bool,
    optShow :: Bool,
    optVersion :: Bool,
    optTable :: Bool,
    optFold :: Bool,
    optFolds :: Int,
    optTableArgs :: Maybe String
  }
  deriving (Show)

defaultOptions =
  Options
    { optInteractive = False,
      optHelpPage = False,
      optBalancing = False,
      optLatex = False,
      optChemfig = False,
      optConsole = True,
      optShow = False,
      optVersion = False,
      optTable = False,
      optFold = False,
      optFolds = 0,
      optTableArgs = Nothing
    }

options :: [OptDescr (Options -> Options)]
options =
  [ Option
      ['i', 'I']
      ["interactive"]
      (NoArg (\opts -> opts {optInteractive = True}))
      "Use STDIN instead of console arguments",
    Option
      ['b', 'B']
      ["balance"]
      (NoArg (\opts -> opts {optBalancing = True}))
      "Unsubscripted Numbers gets treated as multipliers, arrays of arguments also get accepted with symbols other than parenthesis acting as delimeters",
    Option
      ['l', 'L']
      ["latex"]
      (NoArg (\opts -> opts {optLatex = True}))
      "Send output as a latex formula",
    Option
      ['C']
      ["chemfig"]
      (NoArg (\opts -> opts {optChemfig = True}))
      "Send latex output as Chemfig",
    Option
      ['c']
      ["console"]
      (NoArg (\opts -> opts {optConsole = True}))
      "Sent output as unicode",
    Option
      ['s', 'S']
      ["show"]
      (NoArg (\opts -> opts {optShow = True}))
      "Show output via actual value in haskell",
    Option
      ['t']
      ["table"]
      (NoArg (\opts -> opts {optInteractive = True}))
      "Send output as GNU org-mode table",
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
      ['T']
      ["tableconts"]
      ( OptArg
          ((\f opts -> opts {optTableArgs = Just f}) . fromMaybe "input")
          "Table Contents"
      )
      "Put contents of Table Heads"
  ]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: [OPTION...] formula..."
