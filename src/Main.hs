{-
    Projekt: BKG-2-CNF
    Autor: Maroš Geffert <xgeffe00@vutbr.cz>
    Login: xgeffe00
    Rok: 2022
-}
module Main where

import Model.Grammar
import Utils
import Parser.Grammar (parseInputGrammar)
import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)
import NonSimpleRules(nonSimpleGrammar)
import CnfRules (createCNFgrammar)

main :: IO ()
main = do
  arguments <- getArgs
  action <- processArguments arguments
  input <- getInput arguments
  case action of
      Just grammarAction -> either exitHandler (putStrLn . printGramar . grammarAction) $ parseInputGrammar input
      Nothing -> exitHandler "Neznáma akcia"

-- Výber akcie
getAction mode 
    | mode == "-i" = Just originalGrammar
    | mode == "-1" = Just nonSimpleGrammar
    | mode == "-2" = Just createCNFgrammar
    | otherwise = Nothing 

processArguments :: [[Char]] -> IO (Maybe (Grammar -> Grammar))
processArguments (mode:command) = do 
    let input = getAction mode
    return input

-- Zviazanie funkcie s input (akcia -i)
originalGrammar :: Grammar -> Grammar
originalGrammar input = input 
