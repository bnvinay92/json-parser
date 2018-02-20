module JsonParser where

-- r/WatchPeopleCode
-- youtube
import           Data.Char
import           Data.Functor (($>))
import           Json
import           Parser

type JsonParser = Parser String Json

one c = satisfy (== c)

seqP ""     = pure ""
seqP (c:cs) = (:) <$> one c <*> seqP cs

jvalue :: JsonParser
jvalue = jtrue <|> jfalse <|> jnull <|> jarray

jobject :: JsonParser
jobject = JObject <$> (empty <|> nonEmpty)
  where
    empty = (one '{' *> spaces *> one '}') $> []
    nonEmpty = one '{' *> members `surroundedBy` spaces <* one '}'
    members = (:) <$> pair <*> many commaPair
    pair = pure f <*> (string <* colon) <*> jvalue
    f s json = (s, json)
    colon = one ':' `surroundedBy` spaces
    commaPair = comma *> pair

string = many char `surroundedBy` one '"'

char = undefined

jarray :: JsonParser
jarray = JArray <$> (empty <|> nonEmpty)
  where
    empty = (one '[' *> spaces *> one ']') $> []
    nonEmpty = one '[' *> elements `surroundedBy` spaces <* one ']'
    elements = (:) <$> jvalue <*> many commaValue
    commaValue = comma *> jvalue

comma = one ',' `surroundedBy` spaces

surroundedBy p junk = junk *> p <* junk

spaces = many (satisfy isSpace)

jtrue :: JsonParser
jtrue = seqP "true" $> JTrue

jfalse :: JsonParser
jfalse = seqP "false" $> JFalse

jnull :: JsonParser
jnull = seqP "null" $> JNull
