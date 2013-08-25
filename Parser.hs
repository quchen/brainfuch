module Parser (
      parseBrainfuck
) where

import Text.Parsec hiding ((<|>))
import Text.Parsec.String
import Data.Functor
import Data.Maybe (mapMaybe)
import Control.Applicative hiding (many, optional)

import Types


parseBrainfuck :: String -> Either ParseError BrainfuckSource
parseBrainfuck = fmap dropRedundant . parse superfuckP "Brainfuck source parser"


-- / Removes redundant statements like `Add 0`.
dropRedundant :: BrainfuckSource -> BrainfuckSource
dropRedundant (SFSource xs) = SFSource $ mapMaybe dropRedundant' xs
      where dropRedundant' (Add  0)  = Nothing
            dropRedundant' (Move 0)  = Nothing
            dropRedundant' (Loop ys) = Just . Loop $ dropRedundant ys
            dropRedundant' ys        = Just ys



superfuckP :: Parser BrainfuckSource
superfuckP = SFSource . concat <$> do
      optional commentP
      many $ many1 superfuckCommandP <* optional commentP



superfuckCommandP :: Parser BrainfuckCommand
superfuckCommandP = moveP
                <|> addP
                <|> printP
                <|> readP
                <|> loopP

-- | General parser for compensating fields, used to define the parser for Add
--   and Move.
compensatingP :: (Int -> BrainfuckCommand) -- ^ Data constructor
              -> Char                      -- ^ "+1" character (i.e. > and +)
              -> Char                      -- ^ "-1" character (i.e. < and -)
              -> Parser BrainfuckCommand
compensatingP c more less = c . sum <$> many1 (moreP <|> lessP)
      where moreP = char more *> pure   1
            lessP = char less *> pure (-1)



moveP :: Parser BrainfuckCommand
moveP = compensatingP Move '>' '<'



addP :: Parser BrainfuckCommand
addP = compensatingP Add '+' '-'



printP :: Parser BrainfuckCommand
printP = Print . sum <$> many1 dotP
      where dotP = char '.' *> pure 1



readP :: Parser BrainfuckCommand
readP = Read <$ char ','



commentP :: Parser ()
commentP = many1 (noneOf "+-<>,.[]") *> pure ()



loopP :: Parser BrainfuckCommand
loopP = Loop <$> between (char '[') (char ']') superfuckP
