# calculator

simple interpreter


### test code of Parsec

```haskell
simple :: Parser Char
simple = letter

openClose :: Parser Char
openClose = char '(' >> char ')'

parens :: Parser String
parens = (do
    open <- char '('
    value <- parens
    close <- char ')'
    next <- parens
    return $ open : value ++ close : next)
    <|> return ""

nesting :: Parser Int
nesting = do{ char '('
            ; n <- nesting
            ; char ')'
            ; max (n+1) <$> nesting
            }
        <|> return 0

word :: Parser String
word = many1 (letter <?> "") <?> "word"

sentence :: Parser [String]
sentence = do
    words <- sepBy1 word separator
    _ <- oneOf ".?!" <?> "end of sentence"
    return words

separator :: Parser ()
separator = skipMany1 (space <|> char ',' <?> "")

testOr :: Parser String
testOr = string "(a)"
     <|> string "(b)"

testOr1 :: Parser String
testOr1 = sequence [char '(', char 'a' <|> char 'b', char ')']

testOr2 :: Parser String
testOr2 = try (string "(a)")
      <|> string "(b)"
```