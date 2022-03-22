{-
    Projekt: BKG-2-CNF
    Autor: Maroš Geffert <xgeffe00@vutbr.cz>
    Login: xgeffe00
    Rok: 2022
-}
module NonSimpleRules where

import Utils (isOneNonTerminal)
import Model.Grammar (NonTerminal, Symbol, Rule(..), Grammar(..))
import Data.List (intercalate)
import Data.Char ()
import Data.Foldable (Foldable(toList))

-- Takes input grammar and convert it to Grammar without simple rules
nonSimpleGrammar :: Grammar -> Grammar
nonSimpleGrammar grammar =
    Grammar
    (nonTerminals grammar)
    (terminals grammar)
    (startNonTerminal grammar)
    nonSimpleRules
    where
        nonSimpleRules = concatMap (\nonTerm -> nonSimpleRulesForNonTerm nonTerm (rules grammar)) (nonTerminals grammar) 

-- Vytvorí nové pravidlá (bez jednoduchých pravidiel) pre daný nonTerminál
nonSimpleRulesForNonTerm :: [Symbol] -> [Rule] -> [Rule]
nonSimpleRulesForNonTerm nonTerm rules = 
    let nonSimpleRules = filter 
                      (\rule -> 
                        not(isOneNonTerminal (rightSide rule)) && 
                        leftSide rule `elem` createSetA rules nonTerm
                      ) rules
    in map (Rule nonTerm . rightSide) nonSimpleRules

-- Vytvára sa množina N_a (Teda zo štartovného N zbiera všetky samostatné neterminály v ceste) 
createSetA :: [Rule] -> [NonTerminal] -> [[NonTerminal]]
createSetA rules [] = []
createSetA rules nonTerm = nonTerm : createSetA rules expandSetA
  where
    expandSetA = intercalate "" $ map rightSide (filter 
        (\rule ->
            isOneNonTerminal (rightSide rule) &&
            nonTerm == leftSide rule
        )rules)
