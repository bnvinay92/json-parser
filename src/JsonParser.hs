module JsonParser where

import           Data.Functor (($>))
import           Json
import           Parser

type JsonParser = Parser String Json

one c = satisfy (== c)

seqP ""     = pure ""
seqP (c:cs) = (:) <$> one c <*> seqP cs

jvalue :: JsonParser
jvalue = jtrue <|> jfalse <|> jnull

-- "true"
jtrue :: JsonParser
jtrue = seqP "true" $> JTrue

-- "false"
jfalse :: JsonParser
jfalse = seqP "false" $> JFalse

-- "null"
jnull :: JsonParser
jnull = seqP "null" $> JNull
