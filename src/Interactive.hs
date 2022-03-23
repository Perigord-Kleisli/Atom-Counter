module Interactive where

import Args
import System.Console.Haskeline

import Parser
import Control.Monad.IO.Class
import Data.Maybe
import Process
import ToString
--import Data.Version as V
--import Paths_Atom_Counter
import Data.Either
import Control.Lens

lobby :: (Options, [String]) -> Either (IO String) String
lobby
  opts@( Options
      { _optInteractive = isInteractive
      , _optTable = isTableMode
      , _optLatex = showLatex
      , _optShow = showHaskCode
      , _optVersion = showVersion
      , _optFold = maxFold
      , _optToIn = showIn
      , _optFolds = numOfFolds
      , _optSub = showSub
      },
      formula
    )
    | sum (fromEnum <$> [showHaskCode, showLatex, showIn, showSub]) > 1 = Left $ return "Error: output method mixing"
    | isInteractive = Left $ interactive $ (fst opts) {_optInteractive = False}
    | isTableMode = maybe (Left $ return "UserError: Table Parse Error") (Right . orgTable . snd) $ runParser reactProdP $ concat formula
    --         | showVersion = Right $ "Version: " ++ V.showVersion version
    | showHaskCode = maybe message  (Right . show) $ runParser parser $ concat formula
    | otherwise = maybe message (Right . concatMap (toString showFunc) . snd) $ runParser parser $ concat formula
    where
      message = Left $ return "UserError: Parse Error"
      showFunc
        | showSub = toSub
        | showLatex = toLatex
        | otherwise = toBasic

      parser :: Parser [Element]
      parser = if maxFold
        then flattenForm <$> formulaP
        else foldr (.) id (replicate numOfFolds (concatMap foldForm)) . groupForm <$> formulaP


interactive :: Options -> IO String
interactive opts = runInputT defaultSettings $ loop opts
  where
       loop :: Options -> InputT IO String
       loop opts = do
           minput <- getInputLine "AtomC> "
           case minput of
               Nothing -> return "Unexpected exit"
               Just ":q" -> return "Exited Program"
               Just ":t" -> loop $ opts & optTable .~ True
               Just ":l" -> loop $ opts & optLatex .~ True & optShow .~ False & optSub .~ False & optToIn .~ False
               Just ":s" -> loop $ opts & optLatex .~ False & optShow .~ True & optSub .~ False & optToIn .~ False
               Just ":f" -> loop $ opts & optFold .~ True
               Just ":F" -> do 
                          x <- runInputT defaultSettings $ getInputLine "Set fold amount:" 
                          case (maybeRead =<< x :: Maybe Int) of
                            Nothing -> loop opts
                            Just n -> loop $ opts & optFolds .~ n

               Just ":h" -> outputStrLn helpPage >> loop opts
               Just ":i" -> outputStrLn (show opts) >> loop opts
               Just ":d" -> loop $ opts & optLatex .~ False & optShow .~ False & optSub .~ True & optToIn .~ False

               Just (':':x) -> outputStrLn ("Unknown command: '" ++ x ++ "'") >> loop opts

               Just input -> do outputStrLn $ fromRight "Parse Error" $ lobby (opts, [input])
                                loop opts

       helpPage = unlines
         [":q -- Quit"
         ,":t -- Table Output"
         ,":l -- Latex Output"
         ,":s -- Raw Output"
         ,":f -- Fold formula to max"
         ,":F -- Specify number of folds to do"
         ,":h -- Help page"
         ,":d -- Subscript Output"
         ,":i -- Print options"
         ]

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
