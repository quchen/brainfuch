module Parser (parseBrainfuck) where



import Data.Foldable
import Text.Parsec        hiding ((<|>))
import Text.Parsec.String

import Types



parseBrainfuck :: String -> Either ParseError BrainfuckSource
parseBrainfuck = parse (sourceP <* eof) "Brainfuck source parser"

sourceP :: Parser BrainfuckSource
sourceP = fmap BFSource (commentP *> many1 (commandP <* commentP))

commandP :: Parser BrainfuckCommand
commandP = asum
    [ Move   1  <$ char '>'
    , Move (-1) <$ char '<'

    , Add    1  <$ char '+'
    , Add  (-1) <$ char '-'

    , Print  1  <$ char '.'
    , Read      <$ char ','

    , Loop      <$> between (char '[') (char ']') sourceP
    ]

commentP :: Parser ()
commentP = skipMany (noneOf "+-<>,.[]")
