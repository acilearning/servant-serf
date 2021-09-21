{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Foo.Handler.GoodbyeWorld where

import Foo
import qualified Servant
import Servant.API
import Data.Text (Text)

type Route = "goodbyeworld" :> Get '[PlainText] Text

handler :: FooT Servant.Handler Text
handler = do
  pure "Goodbye, World!"
