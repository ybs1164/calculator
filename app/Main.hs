{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main where

import Text.Parsec.String (Parser)
import Text.Parsec (
    parse, string, letter, char, (<?>), many1, digit, choice
  )

type Var = String

data Exp =
    Const Int
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Var Var
  | Let Var Exp Exp
  | If Exp Exp Exp
  | Iszero Exp
  deriving Show

word :: Parser String
word = many1 letter <?> "word"

varExp :: Parser Exp
varExp = Var <$> word

constExp :: Parser Exp
constExp = do
    d <- many1 digit <?> "digit"
    return $ Const (read d)


opExp :: Char -> (Exp -> Exp -> Exp) -> Parser Exp
opExp c exptype = do
    left <- startExp
    char c
    exptype left <$> startExp

addExp :: Parser Exp
addExp = opExp '+' Add

subExp :: Parser Exp
subExp = opExp '-' Sub

mulExp :: Parser Exp
mulExp = opExp '*' Mul

divExp :: Parser Exp
divExp = opExp '/' Div


parensExp :: Parser Exp
parensExp = do
    char '('
    expression <- startExp
    char ')'
    return expression

letExp :: Parser Exp
letExp = do
    string "let"
    w <- word
    char '='
    define <- startExp
    string "in"
    Let w define <$> startExp

ifExp :: Parser Exp
ifExp = do
    string "if"
    condition <- startExp
    string "then"
    dothen <- startExp
    string "else"
    If condition dothen <$> startExp

iszeroExp :: Parser Exp
iszeroExp = do
    string "iszero"
    Iszero <$> startExp


startExp :: Parser Exp
startExp = choice [letExp, ifExp, iszeroExp, firstExp]

firstExp :: Parser Exp
firstExp = choice [addExp, subExp, secondExp]

secondExp :: Parser Exp
secondExp = choice [mulExp, divExp, valueExp]

valueExp :: Parser Exp
valueExp = choice [constExp, varExp, parensExp]


run :: Show a => Parser a -> String -> IO ()
run p input =
    case parse p "" input of
        Left err -> do
            putStr "parse error at "
            print err
        Right x  -> print x

main :: IO ()
main = putStrLn "Hello, Haskell!"
