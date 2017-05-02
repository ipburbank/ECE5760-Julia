import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Combinator
import Data.Functor

-- Taken From https://gist.github.com/m00nlight/2868363ec217f97072b4

data Exp = Num Int
         | Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
         | Div Exp Exp
         | Pow Exp Int
         | Pos Exp
         | Neg Exp deriving (Show)

expr = buildExpressionParser table factor <?> "expr"


table = [[prefix "-" (Neg), prefix "+" (Pos)]
        ,[op "*" (Mul) AssocRight, op "/" (Div) AssocRight, op "^" (Pow) AssocRight]
        ,[op "+" (Add) AssocLeft, op "-" (Sub) AssocLeft]]
        where op s f assoc = Infix (f <$ string s) assoc
              prefix s f = Prefix (f <$ string s)


factor = between (char '(') (char ')') expr
         <|> (Num . read <$> many1 digit) <?> "factor"


eval :: (Num a, Integral a) => Exp -> a
eval e = case e of
    Num x   -> fromIntegral x
    Pos a   -> eval a
    Neg a   -> negate $ eval a
    Add a b -> eval a   +   eval b
    Sub a b -> eval a   -   eval b
    Mul a b -> eval a   *   eval b
    Div a b -> eval a `div` eval b
    Pow a b -> eval a   ^   eval b

solution :: (Num a, Integral a) => String -> a
solution = either (error . show) eval . parse expr ""


main = do
  line <- getLine
  -- putStrLn $ show $ solution (filter (/=' ') line)
  putStrLn $ show $ parse expr "" (filter (/=' ') line)
