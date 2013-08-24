module Parser (
      parseBrainfuck
) where

import Text.Parsec hiding ((<|>))
import Text.Parsec.String
import Data.Functor
import Data.Maybe (mapMaybe)
import Control.Applicative hiding (many, optional)

import Types


parseBrainfuck :: String -> Either ParseError SuperfuckSource
parseBrainfuck = fmap dropRedundant . parse superfuckP "Brainfuck source parser"


-- / Removes redundant statements like `Add 0`.
dropRedundant :: SuperfuckSource -> SuperfuckSource
dropRedundant (SFSource xs) = SFSource $ mapMaybe dropRedundant' xs
      where dropRedundant' (Add  0)  = Nothing
            dropRedundant' (Move 0)  = Nothing
            dropRedundant' (Loop ys) = Just . Loop $ dropRedundant ys
            dropRedundant' ys        = Just ys



superfuckP :: Parser SuperfuckSource
superfuckP = SFSource . concat <$> do
      optional commentP
      many $ many1 superfuckCommandP <* optional commentP



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
compensatingP c more less = c . sum <$> many1 (moreP <|> lessP)
      where moreP = char more *> pure   1
            lessP = char less *> pure (-1)



moveP :: Parser SuperfuckCommand
moveP = compensatingP Move '>' '<'



addP :: Parser SuperfuckCommand
addP = compensatingP Add '+' '-'



printP :: Parser SuperfuckCommand
printP = Print . sum <$> many1 dotP
      where dotP = char '.' *> pure 1



readP :: Parser SuperfuckCommand
readP = Read <$ char ','



commentP :: Parser ()
commentP = many1 (noneOf "+-<>,.[]") *> pure ()



loopP :: Parser SuperfuckCommand
loopP = Loop <$> between (char '[') (char ']') superfuckP
