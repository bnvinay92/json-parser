module JsonParser where

import           Control.Applicative
import           Data.Char
import           Data.Functor        (($>))
import           Json
import           Parser

type JsonParser = Parser String Json

one c = satisfy (== c)

seqP ""     = pure ""
seqP (c:cs) = one c `prepend` seqP cs

prepend p ps = (:) <$> p <*> ps

jvalue :: JsonParser
jvalue = jtrue <|> jfalse <|> jnull <|> jstring <|> jarray

jarray :: JsonParser
jarray = JArray <$> (one '[' *> commaSeparated jvalue <* one ']')

jobject :: JsonParser
jobject = JObject <$> (one '{' *> commaSeparated pair <* one '}')
  where
    pair = toTuple <$> (string <* colon) <*> jvalue
    colon = one ':' `surroundedBy` spaces
    toTuple str val = (str, val)

commaSeparated :: Parser String o -> Parser String [o]
commaSeparated p = commaSeparated' `surroundedBy` spaces
  where
    commaSeparated' = (p `prepend` many commaP) <|> pure []
    commaP = comma *> p
    comma = one ',' `surroundedBy` spaces

spaces = many (satisfy isSpace)

surroundedBy p junk = junk *> p <* junk

jstring :: JsonParser
jstring = JString <$> string

string :: Parser String String
string = many char `surroundedBy` one '"'
  where
    char = satisfy isChar <|> one '\\' *> (control <|> unicode)
    isChar c = not $ any (\f -> f c) [(== '"'), (== '\\'), isControl]
    control = foldr ((<|>) . one) empty ['"', '\\', '/', '\b', '\f', '\n', '\r', '\t']
    unicode = chr <$> (one 'u' *> fourHexDigits)
    hexDigit = satisfy isHexDigit
    fourHexDigits = toHex <$> replicateP 4 hexDigit
    replicateP 0 p = pure []
    replicateP n p = p `prepend` replicateP (n - 1) p
    toHex hexDigits = hexDigitsToInt $ map digitToInt hexDigits
    hexDigitsToInt = foldl (\num d -> num * 16 + d) 0

jtrue :: JsonParser
jtrue = seqP "true" $> JTrue

jfalse :: JsonParser
jfalse = seqP "false" $> JFalse

jnull :: JsonParser
jnull = seqP "null" $> JNull
