module Data.Primitive.Maybe.Internal
  ( nothingSurrogate
  ) where

import GHC.Exts (Any)

nothingSurrogate :: Any
nothingSurrogate = error "nothingSurrogate: This value should not be forced!"
{-# NOINLINE nothingSurrogate #-}

