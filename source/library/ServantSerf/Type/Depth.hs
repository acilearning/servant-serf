module ServantSerf.Type.Depth where

data Depth
  = Deep
  | Shallow
  deriving (Eq, Show)

fromString :: String -> Maybe Depth
fromString x = case x of
  "deep" -> Just Deep
  "shallow" -> Just Shallow
  _ -> Nothing
