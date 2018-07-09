{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Primitive.Maybe.Internal
  ( nothingSurrogate
  , unsafeToMaybe 
  , toAny
  , fromAny
  , anyToFunctor
  , functorToAny

  , createArray
  , createSmallArray
  ) where

import Data.Primitive.Array
import Data.Primitive.SmallArray
import Control.Monad.ST (ST, runST)
import GHC.Exts (Any, reallyUnsafePtrEquality#, Array#, SmallArray#)
import Unsafe.Coerce (unsafeCoerce)

nothingSurrogate :: Any
nothingSurrogate = error "nothingSurrogate: This value should not be forced!"
{-# NOINLINE nothingSurrogate #-}

unsafeToMaybe :: Any -> Maybe a
unsafeToMaybe a =
  case reallyUnsafePtrEquality# a nothingSurrogate of
    1#  -> Nothing
    _ -> Just (fromAny a)
{-# INLINE unsafeToMaybe #-}

toAny :: a -> Any
toAny = unsafeCoerce
{-# INLINE toAny #-}

fromAny :: Any -> a
fromAny = unsafeCoerce
{-# INLINE fromAny #-}

anyToFunctor :: Any -> (a -> b)
anyToFunctor = unsafeCoerce
{-# INLINE anyToFunctor #-}

functorToAny :: (a -> b) -> Any
functorToAny = unsafeCoerce
{-# INLINE functorToAny #-}

-- This low-level business is designed to work with GHC's worker-wrapper
-- transformation. A lot of the time, we don't actually need an Array
-- constructor. By putting it on the outside, and being careful about
-- how we special-case the empty array, we can make GHC smarter about this.
-- The only downside is that separately created 0-length arrays won't share
-- their Array constructors, although they'll share their underlying
-- Array#s.
createArray
  :: Int
  -> a
  -> (forall s. MutableArray s a -> ST s ())
  -> Array a
createArray 0 _ _ = Array (emptyArray# (# #))
createArray n x f = runArray $ do
  mary <- newArray n x
  f mary
  pure mary

emptyArray# :: (# #) -> Array# a
emptyArray# _ = case emptyArray of Array ar -> ar
{-# NOINLINE emptyArray# #-}

emptyArray :: Array a
emptyArray =
  runST $ newArray 0 (error "impossible") >>= unsafeFreezeArray
{-# NOINLINE emptyArray #-}

createSmallArray ::
     Int
  -> a
  -> (forall s. SmallMutableArray s a -> ST s ())
  -> SmallArray a
createSmallArray 0 _ _ = SmallArray (emptySmallArray# (# #))
createSmallArray n x f = runSmallArray $ do
  mary <- newSmallArray n x
  f mary
  pure mary

emptySmallArray# :: (# #) -> SmallArray# a
emptySmallArray# _ = case emptySmallArray of SmallArray ar -> ar
{-# NOINLINE emptySmallArray# #-}

emptySmallArray :: SmallArray a
emptySmallArray = runST $ newSmallArray 0 (error "impossible") >>= unsafeFreezeSmallArray
{-# NOINLINE emptySmallArray #-}
