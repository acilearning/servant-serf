module Regex 
  ( Pattern'
  , patternCodec
  , filterPattern
  , parseRegex'
  ) where

import Data.Text (Text)
import Text.Regex.TDFA (defaultExecOpt, defaultCompOpt, RegexLike(..))
import Text.Regex.TDFA.Pattern
import Text.Regex.TDFA.ReadRegex (parseRegex)
import Text.Regex.TDFA.TDFA ( patternToRegex )
import Text.Regex.TDFA.Text
import qualified Data.Text as T
import qualified Toml

type Pattern' = (Pattern, (GroupIndex, DoPa))

parseRegex' :: Text -> Either Text Pattern'
parseRegex' inp = case parseRegex (T.unpack inp) of
  Left e -> Left $ T.pack $ show e
  Right pattern -> Right pattern

_Pattern :: Toml.TomlBiMap Pattern' Toml.AnyValue
_Pattern = Toml._TextBy (T.pack . showPattern . fst) parseRegex'

patternCodec :: Toml.Key -> Toml.TomlCodec Pattern'
patternCodec key = Toml.match _Pattern key

filterPattern :: Pattern' -> [Text] -> [Text]
filterPattern pattern strings = filter matchPattern strings
  where
  regex :: Regex
  regex = patternToRegex pattern defaultCompOpt defaultExecOpt
  matchPattern :: Text -> Bool
  matchPattern x = matchTest regex x