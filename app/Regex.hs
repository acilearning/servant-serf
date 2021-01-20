module Regex 
  ( Pattern'
  , filterPattern
  , parseRegex
  ) where

import Data.Text (Text)
import Text.Regex.TDFA (defaultExecOpt, defaultCompOpt, RegexLike(..))
import Text.Regex.TDFA.Pattern
import Text.Regex.TDFA.ReadRegex (parseRegex)
import Text.Regex.TDFA.TDFA ( patternToRegex )
import Text.Regex.TDFA.Text

type Pattern' = (Pattern, (GroupIndex, DoPa))

filterPattern :: Pattern' -> [Text] -> [Text]
filterPattern pattern strings = filter matchPattern strings
  where
  regex :: Regex
  regex = patternToRegex pattern defaultCompOpt defaultExecOpt
  matchPattern :: Text -> Bool
  matchPattern x = matchTest regex x
