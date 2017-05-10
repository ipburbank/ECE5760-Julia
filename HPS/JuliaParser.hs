{-# LANGUAGE FlexibleContexts #-}

module JuliaParser(Exp(..), juliaParse) where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Combinator
import Data.Functor

-- Taken From https://gist.github.com/m00nlight/2868363ec217f97072b4

data Exp = Num Int
         | Var String
         | Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
         | Div Exp Exp
         | Pow Exp Exp
         | Pos Exp
         | Neg Exp
           deriving (Show)

--------------------- PARSER ---------------------
expr = buildExpressionParser table factor <?> "expr"

--table :: OperatorTable s u m Exp
table = [[prefix "-" (Neg), prefix "+" (Pos)]
        ,[op "^" (Pow) AssocRight]
        ,[op "*" (Mul) AssocRight, op "/" (Div) AssocRight]
        ,[op "+" (Add) AssocLeft, op "-" (Sub) AssocLeft]]
        where op s f assoc = Infix (f <$ string s) assoc
              prefix s f = Prefix (f <$ string s)


factor = between (char '(') (char ')') expr
         <|> (Var <$> many1 letter)
         <|> (Num . read <$> many1 digit)
         <?> "factor"

juliaParse line = parse JuliaParser.expr "" (filter (/=' ') line)
