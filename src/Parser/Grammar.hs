{-
    Projekt: BKG-2-CNF
    Autor: Maroš Geffert <xgeffe00@vutbr.cz>
    Login: xgeffe00
    Rok: 2022
-}
module Parser.Grammar where

import Model.Grammar
    ( Terminal, NonTerminal, Rule(Rule), Grammar(Grammar) )
import Data.Char ()
import Data.List (intercalate)
import Text.ParserCombinators.Parsec
    ( char,
      letter,
      lower,
      newline,
      string,
      upper,
      count,
      eof,
      many1,
      sepBy1,
      sepEndBy,
      parse,
      Parser )

parseInputGrammar :: String -> Either String Grammar
parseInputGrammar input = case parse grammarInFile "" input of
    Left err -> Left "Nevalidný format gramatiky"
    Right grammar -> if isGrammarValid grammar then Right grammar else Left "Nevalidný formát gramatiky"

-- Parsovanie gramatiky zo vstupného súboru
grammarInFile :: Parser Grammar
grammarInFile =
  Grammar 
  <$> 
    (
        parseNonTerminals <* newline
    ) 
  <*> 
    (
        parseTerminals <* newline
    ) 
  <*>
    (
        parseNonTerminal <* newline
    ) 
  <*>  
    (
        rulesParse <* eof
    )

parseNonTerminals :: Parser [[NonTerminal]]
parseNonTerminals = sepBy1 parseNonTerminal comma

parseTerminals :: Parser [[Terminal]]
parseTerminals = sepBy1 (count 1 lower) comma

parseNonTerminal :: Parser [NonTerminal]
parseNonTerminal = many1 upper

ruleParse :: Parser Rule
ruleParse = do
    ls <- parseNonTerminal
    arrow
    rs <- many1 letter
    return (Rule ls rs)

-- Parser rule for parsing all rules in input grammar
rulesParse :: Parser [Rule]
rulesParse = sepEndBy ruleParse newline

isGrammarValid :: Grammar -> Bool
isGrammarValid grammar@(Grammar nonterms terms start rules) =
    let isValidNonTerms = all ((==1) . length) nonterms
        isValidTerms = all ((==1) . length) terms
        isValidStartNonTerm = (length start == 1) && start `elem` nonterms
        isValidRules = all (validRule grammar) rules
    in isValidNonTerms && isValidTerms && isValidStartNonTerm && isValidRules

validRule grammar@(Grammar nonterms terms start rules) rule@(Rule left right) = 
  left `elem` nonterms && length left == 1 && all ( `elem` intercalate "" (terms ++ nonterms)) right

arrow :: Parser String
arrow = string "->"

comma :: Parser Char
comma = char ','
