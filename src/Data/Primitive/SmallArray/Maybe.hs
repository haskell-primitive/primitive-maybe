{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RoleAnnotations #-}

-- | This provides an interface to working with boxed arrays
-- with elements of type @Maybe a@. That is:
--
-- > SmallMaybeArray a ≅ SmallArray (Maybe a)
--
-- However, this type provided by this module is more efficient
-- than its naive @SmallArray@ counterpart. It consumes less
-- memory and has fewer heap indirections.
module Data.Primitive.SmallArray.Maybe
  ( SmallMaybeArray
  , SmallMutableMaybeArray
  , indexSmallMaybeArray
  , newSmallMaybeArray
  , readSmallMaybeArray
  , writeSmallMaybeArray
  , sequenceSmallMaybeArray
  , unsafeFreezeSmallMaybeArray
  ) where

import Control.Monad.Primitive
import Data.Primitive.SmallArray

import Data.Primitive.Maybe.Internal (nothingSurrogate)
import GHC.Exts (Any,reallyUnsafePtrEquality#)
import Unsafe.Coerce (unsafeCoerce)

newtype SmallMaybeArray a = SmallMaybeArray (SmallArray Any)
newtype SmallMutableMaybeArray s a = SmallMutableMaybeArray (SmallMutableArray s Any)

type role SmallMaybeArray representational
type role SmallMutableMaybeArray nominal representational

unsafeToMaybe :: Any -> Maybe a
unsafeToMaybe a =
  case reallyUnsafePtrEquality# a nothingSurrogate of
    0# -> Just (unsafeCoerce a)
    _  -> Nothing
{-# INLINE unsafeToMaybe #-}

newSmallMaybeArray :: PrimMonad m => Int -> Maybe a -> m (SmallMutableMaybeArray (PrimState m) a)
{-# INLINE newSmallMaybeArray #-}
newSmallMaybeArray i ma = case ma of
  Just a -> do
    x <- newSmallArray i (unsafeCoerce a)
    return (SmallMutableMaybeArray x)
  Nothing -> do
    x <- newSmallArray i nothingSurrogate
    return (SmallMutableMaybeArray x)

indexSmallMaybeArray :: SmallMaybeArray a -> Int -> Maybe a
{-# INLINE indexSmallMaybeArray #-}
indexSmallMaybeArray (SmallMaybeArray a) ix =
  let (# v #) = indexSmallArray## a ix
   in unsafeToMaybe v

readSmallMaybeArray :: PrimMonad m => SmallMutableMaybeArray (PrimState m) a -> Int -> m (Maybe a)
{-# INLINE readSmallMaybeArray #-}
readSmallMaybeArray (SmallMutableMaybeArray m) ix = do
  a <- readSmallArray m ix
  return (unsafeToMaybe a)

writeSmallMaybeArray :: PrimMonad m => SmallMutableMaybeArray (PrimState m) a -> Int -> Maybe a -> m ()
{-# INLINE writeSmallMaybeArray #-}
writeSmallMaybeArray (SmallMutableMaybeArray marr) ix ma = case ma of
  Just a -> writeSmallArray marr ix (unsafeCoerce a)
  Nothing -> writeSmallArray marr ix nothingSurrogate

-- | This is like calling @sequence@ on a 'SmallArray'. However, in
-- the event that all the values are @Just@, it does not need
-- to allocate a new array since the array backing the @SmallMaybeArray@
-- can be reused.
sequenceSmallMaybeArray :: SmallMaybeArray a -> Maybe (SmallArray a)
sequenceSmallMaybeArray m@(SmallMaybeArray a) =
  if hasNothing m then Nothing else Just (unsafeCoerce a)

hasNothing :: SmallMaybeArray a -> Bool
hasNothing (SmallMaybeArray a) = go 0 where
  go !ix = if ix < sizeofSmallArray a
    then
      let (# v #) = indexSmallArray## a ix
       in case reallyUnsafePtrEquality# v nothingSurrogate of
            0# -> True
            _  -> go (ix + 1)
    else False

unsafeFreezeSmallMaybeArray :: PrimMonad m => SmallMutableMaybeArray (PrimState m) a -> m (SmallMaybeArray a)
{-# INLINE unsafeFreezeSmallMaybeArray #-}
unsafeFreezeSmallMaybeArray (SmallMutableMaybeArray ma) = do
  a <- unsafeFreezeSmallArray ma
  return (SmallMaybeArray a)
