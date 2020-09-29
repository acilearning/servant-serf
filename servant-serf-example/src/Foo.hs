{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Foo where

newtype FooT m a = FooT (m a) deriving
  ( Applicative
  , Functor
  , Monad
  )
