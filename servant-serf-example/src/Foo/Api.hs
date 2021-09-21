-- | test Api module

{-# OPTIONS_GHC -F -pgmF servant-serf
  -optF --package-name=servant-serf-example
  -optF --is-handler-module=Foo.Handler.*
#-}

module Foo.Api where

-- the order of these matters

import Foo.Handler.HelloWorld
import Foo.Handler.GoodbyeWorld
