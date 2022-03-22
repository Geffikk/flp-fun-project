{-
    Projekt: BKG-2-CNF
    Autor: Maroš Geffert <xgeffe00@vutbr.cz>
    Login: xgeffe00
    Rok: 2022
-}
module Model.Grammar where

data Grammar = Grammar
    {
        nonTerminals :: [[NonTerminal]],
        terminals :: [[Terminal]],
        startNonTerminal :: [NonTerminal],
        rules :: [Rule]
    } deriving (Show)

data Rule = Rule
    {
        leftSide :: [NonTerminal],
        rightSide :: [Symbol]
    } deriving (Show, Eq)

type Symbol = Char 
type NonTerminal = Char
type Terminal = Char
