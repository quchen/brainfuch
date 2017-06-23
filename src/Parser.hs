module Parser (parseBrainfuck) where



import           Control.Applicative
import           Control.Monad
import           Data.Foldable
import           Data.Text           (Text)
import qualified Data.Text           as T

import Types



parseBrainfuck :: Text -> Maybe BrainfuckSource
parseBrainfuck = parseUnique (sourceP <* eof)

sourceP :: Parser BrainfuckSource
sourceP = fmap BFSource (commentP *> some (commandP <* commentP))

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
commentP = many (noneOf "+-<>,.[]") *> pure ()

data Parser a = Parser { runParser :: Text -> [(a, Text)] }

parseUnique :: Parser a -> Text -> Maybe a
parseUnique p input = case [result | (result, rest) <- runParser p input, T.null rest] of
    [unique] -> Just unique
    _otherwise -> Nothing

failP :: Parser a
failP = Parser (\_ -> [])

eof :: Parser ()
eof = anyChar *> failP <|> pure ()

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure x = Parser (\input -> [(x, input)])
    (<*>) = ap

instance Monad Parser where
    p >>= f = Parser (\input ->
        concat [runParser (f a) rest
               | (a, rest) <- runParser p input] )

instance Alternative Parser where
    empty = failP
    p1 <|> p2 = Parser (\input -> runParser p1 input ++ runParser p2 input)

anyChar :: Parser Char
anyChar = Parser $ \input -> case T.uncons input of
    Nothing -> []
    Just (c,cs) -> [(c, cs)]

between :: Parser a -> Parser b -> Parser c -> Parser c
between before after p = do
    _ <- before
    result <- p
    _ <- after
    pure result

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
    c <- anyChar
    if p c then pure c
           else failP

char :: Char -> Parser Char
char x = satisfy (== x)

noneOf :: [Char] -> Parser Char
noneOf forbidden = satisfy (`notElem` forbidden)
