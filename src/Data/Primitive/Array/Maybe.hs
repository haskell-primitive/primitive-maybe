{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RoleAnnotations #-}

-- | This provides an interface to working with boxed arrays
-- with elements of type @Maybe a@. That is:
--
-- > MaybeArray a ≅ Array (Maybe a)
--
-- However, this type provided by this module is more efficient
-- than its naive @Array@ counterpart. It consumes less
-- memory and has fewer heap indirections.
module Data.Primitive.Array.Maybe
  ( MaybeArray
  , MutableMaybeArray
  , indexMaybeArray
  , newMaybeArray
  , readMaybeArray
  , writeMaybeArray
  , sequenceMaybeArray
  , unsafeFreezeMaybeArray
  , thawMaybeArray
  ) where

import Control.Monad.Primitive
import Data.Primitive.Array

import Data.Primitive.Maybe.Internal (nothingSurrogate)
import GHC.Exts (Any,reallyUnsafePtrEquality#)
import Unsafe.Coerce (unsafeCoerce)

newtype MaybeArray a = MaybeArray (Array Any)
newtype MutableMaybeArray s a = MutableMaybeArray (MutableArray s Any)

type role MaybeArray representational
type role MutableMaybeArray nominal representational

unsafeToMaybe :: Any -> Maybe a
unsafeToMaybe a =
  case reallyUnsafePtrEquality# a nothingSurrogate of
    0# -> Just (unsafeCoerce a)
    _  -> Nothing
{-# INLINE unsafeToMaybe #-}

newMaybeArray :: PrimMonad m => Int -> Maybe a -> m (MutableMaybeArray (PrimState m) a)
{-# INLINE newMaybeArray #-}
newMaybeArray i ma = case ma of
  Just a -> do
    x <- newArray i (unsafeCoerce a)
    return (MutableMaybeArray x)
  Nothing -> do
    x <- newArray i nothingSurrogate
    return (MutableMaybeArray x)

indexMaybeArray :: MaybeArray a -> Int -> Maybe a
{-# INLINE indexMaybeArray #-}
indexMaybeArray (MaybeArray a) ix =
  let (# v #) = indexArray## a ix
   in unsafeToMaybe v

readMaybeArray :: PrimMonad m => MutableMaybeArray (PrimState m) a -> Int -> m (Maybe a)
{-# INLINE readMaybeArray #-}
readMaybeArray (MutableMaybeArray m) ix = do
  a <- readArray m ix
  return (unsafeToMaybe a)

writeMaybeArray :: PrimMonad m => MutableMaybeArray (PrimState m) a -> Int -> Maybe a -> m ()
{-# INLINE writeMaybeArray #-}
writeMaybeArray (MutableMaybeArray marr) ix ma = case ma of
  Just a -> writeArray marr ix (unsafeCoerce a)
  Nothing -> writeArray marr ix nothingSurrogate

-- | This is like calling @sequence@ on an 'Array'. However, in
-- the event that all the values are @Just@, it does not need
-- to allocate a new array since the array backing the @MaybeArray@
-- can be reused.
sequenceMaybeArray :: MaybeArray a -> Maybe (Array a)
sequenceMaybeArray m@(MaybeArray a) =
  if hasNothing m then Nothing else Just (unsafeCoerce a)

hasNothing :: MaybeArray a -> Bool
hasNothing (MaybeArray a) = go 0 where
  go !ix = if ix < sizeofArray a
    then
      let (# v #) = indexArray## a ix
       in case reallyUnsafePtrEquality# v nothingSurrogate of
            0# -> True
            _  -> go (ix + 1)
    else False

unsafeFreezeMaybeArray :: PrimMonad m => MutableMaybeArray (PrimState m) a -> m (MaybeArray a)
{-# INLINE unsafeFreezeMaybeArray #-}
unsafeFreezeMaybeArray (MutableMaybeArray ma) = do
  a <- unsafeFreezeArray ma
  return (MaybeArray a)

thawMaybeArray
  :: PrimMonad m
  => MaybeArray a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> m (MutableMaybeArray (PrimState m) a)
thawMaybeArray (MaybeArray a) off len =
  fmap MutableMaybeArray (thawArray a off len)
