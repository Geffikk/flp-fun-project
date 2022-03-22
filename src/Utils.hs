{-
    Projekt: BKG-2-CNF
    Autor: Maroš Geffert <xgeffe00@vutbr.cz>
    Login: xgeffe00
    Rok: 2022
-}
module Utils where

import Model.Grammar
    ( Symbol,
      Rule(leftSide, rightSide),
      Grammar(nonTerminals, terminals, startNonTerminal, rules) )
import Parser.Grammar ()
import Text.ParserCombinators.Parsec ()
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr, openFile, IOMode(ReadMode))
import System.IO.Error(catchIOError, ioeGetFileName, isDoesNotExistError)
import Data.List (intercalate)
import Data.Char (isLower, isUpper)

isOneNonTerminal :: [Symbol] -> Bool
isOneNonTerminal [x] = isUpper x
isOneNonTerminal _ = False

isTwoNonTerminals :: [Symbol] -> Bool
isTwoNonTerminals [x,y] = isOneNonTerminal [x] && isOneNonTerminal [y]
isTwoNonTerminals _ = False

isOneTerminal :: [Symbol] -> Bool
isOneTerminal [x] = isLower x
isOneTerminal _ = False

getInput :: [[Char]] -> IO String
getInput [mode] = getContents
getInput [mode, filename]  = readFile filename `catchIOError` fileHandler

printGramar :: Grammar -> String
printGramar grammar =
    let nonterms = intercalate "," (nonTerminals grammar)
        terms = intercalate "," (terminals grammar)
        start = startNonTerminal grammar
        strRules = printRules (rules grammar)
    in nonterms ++ "\n" ++ terms ++ "\n" ++ start ++ "\n" ++ strRules

printRules :: [Rule] -> String
printRules = concatMap (\a -> leftSide a ++ "->" ++ rightSide a ++ "\n")

fileHandler :: IOError -> IO b
fileHandler e
  | isDoesNotExistError e = do
    case ioeGetFileName e of
      Just path -> hPutStrLn stderr $ "Súbor neexistuje na ceste: " ++ path
      Nothing -> hPutStrLn stderr "Súbor neexistuje"
    exitFailure
  | otherwise = ioError e

exitHandler :: String -> IO a
exitHandler error = do
    hPutStrLn stderr error
    exitFailure
