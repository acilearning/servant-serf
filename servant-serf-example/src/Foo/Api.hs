-- | test Api module

{-# OPTIONS_GHC -F -pgmF servant-serf #-}

module Foo.Api where

-- the order of these matters

import Foo.Handler.HelloWorld
import Foo.Handler.GoodbyeWorld
