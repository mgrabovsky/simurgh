## Punctuation

```haskell
whiteSpace :: Parser ()
```

Parse any white space, i.e. any number of `space`, or comments.

```haskell
parens, brackets, braces, angles :: Parser a -> Parser a
```

Parse a lexeme enclosed in parentheses `()`, brackets `[]`, braces `{}`, or angle
brackets `<>`.

```haskell
colon, comma, semi, dot :: Parser String
```

Parse a colon, comma, semicolon, or dot and skip any trailing white space.

## Lexemes

```haskell
symbol :: String -> Parser String
```

Parse a string and skip trailing white space.

```haskell
lexeme :: Parser a -> Parser a
```

`lexeme p` first applies `p` and then the `whiteSpace` parser. This ensures that the
next parser starts at a point without white space. Every lexical token (lexeme) is
defined this way.

```haskell
commaSep, semiSep :: Parser a -> Parser [a]
```

Parse zero or more occurences of a lexeme separated by a comma/semicolon.

```haskell
commaSep1, semiSep1 :: Parser a -> Parser [a]
```

Parse _one_ or more occurences of a lexeme separated by a comma/semicolon.

## Predefined lexemes

```haskell
identifier, operator :: Parser String
```

Parse a legal identifier/operator. Fail on reserved identifiers/operators.

```haskell
reserved, reservedOp :: Parser String
```

Parse a reserved identifier/operator which is not a prefix of a non-reserved
identifier/operator.

```haskell
charLiteral :: Parser Char
stringLiteral :: Parser String
```

Parse a character or string literal. This parser deals correctly with escape
sequences and gaps.

```haskell
natural :: Parser Integer
integer :: Parser Integer
float :: Parser Double
```

Parse a natural number, an integer or a floating-point value according to the rules
specified in the [Haskell Report](https://www.haskell.org/onlinereport/haskell2010/).

```haskell
naturalOrFloat :: Parser (Either Integer Double)
```

Parse either an integer or a float.

```haskell
decimal, hexadecimal, octal :: Parser Integer
```

Parse a natural number in decimal, hexadecimal (prefixed with `0x` or `0X`), or
octal (prefixed with `0o` or `0O`) base.

## Combinators

```haskell
choice :: [Parser a] -> Parser a
```

Try to apply the parser in sequence until the first success.

```haskell
count :: Int -> Parser a -> Parser [a]
```

`count n p` parses exactly `n > 0` occurrences of `p`.

```haskell
between :: Parser open -> Parser close -> Parser a -> Parser a
```

TODO

```haskell
option :: a -> Parser a -> Parser a
optionMaybe :: Parser a -> Parser (Maybe a)
```

Try to apply the parser and return a default value/Nothing on failure.

```haskell
optional :: Parser a -> Parser ()
```

`optional p` either parses exactly `p` or nothing. It only fails if `p` fails
after consuming input.

```haskell
skipMany1 :: Parser a -> Parser ()
many1 :: Parser a -> Parser [a]
```

Apply a parser _one_ or more times and ignore/collect the results.

```haskell
sepBy, sepBy1 :: Parser a -> Parser sep -> Parser [a]
```

TODO

```haskell
endBy, endBy1 :: Parser a -> Parser sep -> Parser [a]
```

TODO

```haskell
sepEndBy, sepEndBy1 :: Parser a -> Parser sep -> Parser [a]
```

TODO

```haskell
chainl, chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl1, chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
```
TODO

```haskell
eof :: Parser ()
```
Parse end of input.

```haskell
notFollowedBy :: Parser a -> Parser ()
```

TODO

```haskell
manyTill :: Parser a -> Parser end -> Parser [a]
```

`manyTill p end` applies the parser `p` until `end` succeeds, collecting the results.

```haskell
lookAhead :: Parser a -> Parser a
```

Parse without consuming any input. Fail if the inner parser fails with consumed
input.

