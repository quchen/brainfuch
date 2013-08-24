module BrainfuckParser (
      parseBrainfuck
) where

import Text.Parsec hiding ((<|>))
import Text.Parsec.String
import Data.Functor
import Control.Applicative hiding (many)

import Types


parseBrainfuck :: String -> Either ParseError SuperfuckSource
parseBrainfuck = parse superfuckP "Brainfuck source code"


superfuckP :: Parser SuperfuckSource
superfuckP = SFSource <$> sepEndBy superfuckCommandP (commentP <|> eof)

superfuckCommandP :: Parser SuperfuckCommand
superfuckCommandP = moveP
                <|> addP
                <|> printP
                <|> readP
                <|> loopP

-- | General parser for compensating fields, used to define the parser for Add
--   and Move.
compensatingP :: (Int -> SuperfuckCommand) -- ^ Data constructor
              -> Char                      -- ^ "+1" character (i.e. > and +)
              -> Char                      -- ^ "-1" character (i.e. < and -)
              -> Parser SuperfuckCommand
compensatingP c more less = c . sum <$> many (moreP <|> lessP)
      where moreP = char more *> pure   1
            lessP = char less *> pure (-1)

addP :: Parser SuperfuckCommand
addP = compensatingP Add '+' '-'

moveP :: Parser SuperfuckCommand
moveP = compensatingP Move '>' '<'

printP :: Parser SuperfuckCommand
printP = Print . sum <$> many dotP
      where dotP = char '.' *> pure 1

readP :: Parser SuperfuckCommand
readP = Read <$ char ','

commentP :: Parser ()
commentP = many1 (noneOf "+-<>,.[]") *> pure ()

loopP :: Parser SuperfuckCommand
loopP = Loop <$> between (char '[') (char ']') superfuckP