module Parser where

import           Data.Char

-- A parser is a software component that takes input data (frequently text) and builds a data structure
import           Json

newtype Parser i o = Parser
  { parse :: i -> Maybe (o, i)
  }

instance Functor (Parser i) where
  fmap f p =
    Parser $ \inp ->
      case parse p inp of
        Nothing          -> Nothing
        (Just (o, rest)) -> Just (f o, rest)

instance Applicative (Parser i) where
  pure f = Parser $ \inp -> Just (f, inp)
  pf <*> parg =
    Parser $ \inp ->
      case parse pf inp of
        Nothing -> Nothing
        (Just (f, rest)) ->
          case parse parg rest of
            Nothing           -> Nothing
            (Just (a, rest')) -> Just (f a, rest')

(<|>) = orP

orP :: Parser i o -> Parser i o -> Parser i o
orP p1 p2 =
  Parser $ \inp ->
    case parse p1 inp of
      Nothing -> parse p2 inp
      success -> success

satisfy :: (i -> Bool) -> Parser [i] i
satisfy pred =
  Parser $ \inp ->
    case inp of
      (c:cs)
        | pred c -> Just (c, cs)
      _ -> Nothing

many :: Parser i o -> Parser i [o]
many p = ((:) <$> p <*> many p) <|> pure []
