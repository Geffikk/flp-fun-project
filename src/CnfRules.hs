{-
    Projekt: BKG-2-CNF
    Autor: Maroš Geffert <xgeffe00@vutbr.cz>
    Login: xgeffe00
    Rok: 2022
-}
module CnfRules where

import Model.Grammar
    ( NonTerminal, Symbol, Rule(Rule, leftSide), Grammar(..) )
import Utils ( isOneNonTerminal, isTwoNonTerminals, isOneTerminal )
import Parser.Grammar ()
import Data.List (intercalate, union)
import Data.Char ()
import NonSimpleRules ( nonSimpleGrammar )

-- korektné dĺžky pre cnf pravidlá
isCNFOne :: (Eq a, Num a) => a -> Bool
isCNFOne x = x == 1
isCNFTwo :: (Eq a, Num a) => a -> Bool
isCNFTwo x = x == 2
notCNF :: (Ord a, Num a) => a -> Bool
notCNF x = x > 2

createCNFgrammar :: Grammar -> Grammar
createCNFgrammar grammar =
  Grammar
    (cnfNonTerms' `union` nonTerminals grammar)
    (terminals grammar)
    (startNonTerminal grammar)
    cnfRules'
  where
    -- Before converting to CNF, Grammar without simple rules is required
    nonSimpleBKG = nonSimpleGrammar grammar
    -- Create CNF rules
    cnfRules = concatMap (`convertToCnfForm` nonSimpleBKG) (rules nonSimpleBKG)
    -- Vymaž všetky duplikáty z neterminálov
    cnfNonTerms' = removeDuplicates
      where
        removeDuplicates = foldl (\seen x -> if leftSide x `elem` seen then seen else seen ++ [leftSide x]) [] cnfRules
    -- Vymaž všetky duplikáty z pravidiel
    cnfRules' = removeDuplicates
      where
        removeDuplicates = foldl (\seen x -> if x `elem` seen then seen else seen ++ [x]) [] cnfRules

convertToCnfForm :: Rule -> Grammar -> [Rule]
convertToCnfForm rule@(Rule left right) grammar
  -- Samotný terminál už je v CNF forme 
  -- (Samotný neterminál už bol odstránený pri algoritme na redukciu jednoduchých pravidiel)
  | isOneTerminal right = [rule]
  | isCNFTwo (length right) && isTwoNonTerminals right = [rule]
  | isCNFTwo (length right) && isOneNonTerminal [head right] = processNonTermAndTerm rule grammar
  | isCNFTwo (length right) && isOneTerminal [head right] && isOneTerminal (tail right) = processTermAndTerm rule grammar
  | isCNFTwo (length right) && isOneTerminal [head right] && isOneNonTerminal (tail right) = processTermAndNonTerm rule grammar
  | notCNF (length right) && isOneNonTerminal [head right] = processNonTermAndRest rule grammar
  | otherwise  = processTermAndRest rule grammar

-- N -> N T => N -> N N (N: NonTerminal, T: Terminal => z N,T sa vytvoria nové N N)
processNonTermAndTerm :: Rule -> Grammar -> [Rule]
processNonTermAndTerm rule@(Rule left (h:t)) grammar =
    let ruleFromRest = Rule (nonTermNameFromTerm t) t
    in [Rule left (h : nonTermNameFromTerm t), ruleFromRest]

 -- N -> T T => N -> N N (N: NonTerminal, T: Terminal => z N,T sa vytvoria nové N N)
processTermAndTerm :: Rule -> Grammar -> [Rule]
processTermAndTerm rule@(Rule left (h:t)) grammar =
    let ruleFromTerm = Rule (nonTermNameFromTerm [h]) [h]
        ruleFromRest = Rule (nonTermNameFromTerm t) t
    in [Rule left (nonTermNameFromTerm [h] ++ nonTermNameFromTerm t), ruleFromTerm, ruleFromRest]

-- N -> T N => N -> N N (N: NonTerminal, T: Terminal => z N,T sa vytvoria nové N N)
processTermAndNonTerm :: Rule -> Grammar -> [Rule]
processTermAndNonTerm rule@(Rule left (h:t)) grammar =
    let ruleFromTerm = Rule (nonTermNameFromTerm [h]) [h]
    in [Rule left (nonTermNameFromTerm [h] ++ t), ruleFromTerm]

-- N -> N <[N,T]> => N -> N <[N]> (N: NonTerminal, T: Terminal => z N,<[N,T]> sa vytvoria nové N <[N]>)
processNonTermAndRest :: Rule -> Grammar -> [Rule]
processNonTermAndRest rule@(Rule left (x1:right)) grammar =
    let newRules = convertToCnfForm (Rule (nonTermNameFromRest right) right) grammar
    in Rule left (x1 : nonTermNameFromRest right) : newRules

-- N -> T <[N,T]> => N -> N <[N]> (N: NonTerminal, T: Terminal => z T,<[N,T]> sa vytvoria nové N <[N]>)
processTermAndRest :: Rule -> Grammar -> [Rule]
processTermAndRest rule@(Rule left (x1:right)) grammar =
    let newRulesTerm = convertToCnfForm (Rule (nonTermNameFromTerm [x1]) [x1]) grammar
        newRulesRest = convertToCnfForm (Rule (nonTermNameFromRest right) right) grammar
    in [Rule left (nonTermNameFromTerm [x1] ++ nonTermNameFromRest right)] ++ newRulesTerm ++ newRulesRest

nonTermNameFromRest :: [Symbol] -> [NonTerminal]
nonTermNameFromRest expression = "<" ++ expression ++ ">"

nonTermNameFromTerm :: [Symbol] -> [NonTerminal]
nonTermNameFromTerm terminal = terminal ++ "\'"
