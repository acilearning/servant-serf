{-# LANGUAGE PartialTypeSignatures #-}

module Foo.Api (type Route, server) where

import Servant ((:<|>)((:<|>)))
import qualified GHC.Stack as Stack
import qualified Servant

import qualified Foo.Handler.HelloWorld
import qualified Foo.Handler.GoodbyeWorld

type Route
  = Foo.Handler.HelloWorld.Route
  :<|> Foo.Handler.GoodbyeWorld.Route

server :: Stack.HasCallStack => Servant.ServerT Route _
server
  = Foo.Handler.HelloWorld.handler
  :<|> Foo.Handler.GoodbyeWorld.handler
