module Parser (parseBrainfuck) where



import Control.Applicative hiding (many, optional)
import Text.Parsec         hiding ((<|>))
import Text.Parsec.String

import Types



parseBrainfuck :: String -> Either ParseError BrainfuckSource
parseBrainfuck = parse superfuckP "Brainfuck source parser"

superfuckP :: Parser BrainfuckSource
superfuckP = BFSource . concat <$> do
      optional commentP
      many $ many1 superfuckCommandP <* optional commentP

superfuckCommandP :: Parser BrainfuckCommand
superfuckCommandP = moveP
                <|> addP
                <|> printP
                <|> readP
                <|> loopP

moveP :: Parser BrainfuckCommand
moveP = Move <$> (rightP <|> leftP)
      where rightP = char '>' *> pure   1
            leftP  = char '<' *> pure (-1)

addP :: Parser BrainfuckCommand
addP = Add <$> (plusP <|> minusP)
      where plusP  = char '+' *> pure   1
            minusP = char '-' *> pure (-1)

printP :: Parser BrainfuckCommand
printP = Print <$> dotP
      where dotP = char '.' *> pure 1

readP :: Parser BrainfuckCommand
readP = Read <$ char ','

commentP :: Parser ()
commentP = many1 (noneOf "+-<>,.[]") *> pure ()

loopP :: Parser BrainfuckCommand
loopP = Loop <$> between (char '[') (char ']') superfuckP
