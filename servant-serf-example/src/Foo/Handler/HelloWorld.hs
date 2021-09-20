{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Foo.Handler.HelloWorld where

import Foo
import qualified Servant
import Servant.API
import Data.Text (Text)

type Route = "helloworld" :> Get '[PlainText] Text

handler :: FooT Servant.Handler Text
handler = do
  pure "Hello, World!"
