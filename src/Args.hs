{-# LANGUAGE TemplateHaskell #-}

module Args where

import Data.Maybe
import System.Console.GetOpt
import Control.Lens

data Options = Options
  { _optInteractive :: Bool,
    _optHelpPage :: Bool,
    _optTable :: Bool,
    _optLatex :: Bool,
    _optShow :: Bool,
    _optToIn :: Bool,
    _optVersion :: Bool,
    _optFold :: Bool,
    _optFolds :: Int,
    _optSub :: Bool
  }
  deriving (Show)

makeLenses ''Options

defaultOptions =
  Options
    { _optInteractive = False,
      _optHelpPage = False,
      _optTable = False,
      _optLatex = False,
      _optShow = False,
      _optToIn = False,
      _optVersion = False,
      _optFold = False,
      _optFolds = 0,
      _optSub = False
    }

options :: [OptDescr (Options -> Options)]
options =
  [ Option
      ['I']
      ["interactive"]
      (NoArg (\opts -> opts {_optInteractive = True}))
      "Use STDIN instead of console arguments, type :q to quit",
    Option
      ['i']
      ["input"]
      (NoArg (\opts -> opts {_optToIn = True}))
      "Send output as output recognizable by the input",
    Option
      ['t', 'T']
      ["table"]
      (NoArg (\opts -> opts {_optTable = True}))
      "Unsubscripted Numbers gets treated as multipliers, arrays of arguments also get accepted with symbols other than parenthesis acting as delimeters",
    Option
      ['l', 'L']
      ["latex"]
      (NoArg (\opts -> opts {_optLatex = True}))
      "Send output as a latex formula",
    Option
      ['v', 'V']
      ["version"]
      (NoArg (\opts -> opts {_optVersion = True}))
      "Show program version",
    Option
      ['s', 'S']
      ["show"]
      (NoArg (\opts -> opts {_optShow = True}))
      "Show output via actual value in haskell",
    Option
      ['f']
      ["fold"]
      (NoArg (\opts -> opts {_optFold = True}))
      "Repeatedly fold formula until at lowest term",
    Option
      ['F']
      ["folds"]
      ( OptArg
          ((\f opts -> opts {_optFolds = read f :: Int}) . fromMaybe "0")
          "NUMBER"
      )
      "Fold formula a number of times",
    Option
      ['D', 'd']
      ["subScript"]
      (NoArg (\opts -> opts {_optSub = True}))
      "show output in subscript"
  ]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: [OPTION...] formula..."
