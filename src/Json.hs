module Json where

data Json
  = JString String
  | JNumber Double
  | JObject [(String, Json)]
  | JArray [Json]
  | JTrue
  | JFalse
  | JNull
  deriving (Eq, Show)
