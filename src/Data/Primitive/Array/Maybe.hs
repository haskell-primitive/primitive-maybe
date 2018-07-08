{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

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

import Control.Monad (when)
import Control.Monad.Primitive
import Data.Primitive.Array

import Data.Primitive.Maybe.Internal
import GHC.Exts (Any,reallyUnsafePtrEquality#, Int(..))
import Unsafe.Coerce (unsafeCoerce)

newtype MaybeArray a = MaybeArray (Array Any)
newtype MutableMaybeArray s a = MutableMaybeArray (MutableArray s Any)

type role MaybeArray representational
type role MutableMaybeArray nominal representational

instance Functor MaybeArray where
  fmap :: forall a b. (a -> b) -> MaybeArray a -> MaybeArray b
  fmap f (MaybeArray arr) = MaybeArray $
    createArray (sizeofArray arr) (error "impossible") $ \mb ->
      let go i | i == (sizeofArray arr) = return ()
               | otherwise = do
                   x <- indexArrayM arr i
                   case (unsafeToMaybe x :: Maybe a) of
                     Nothing -> writeArray mb i (toAny Nothing :: Any) >> go (i + 1)
                     Just a -> writeArray mb i (toAny (Just (f a))) >> go (i + 1)
      in go 0
  e <$ (MaybeArray a) = MaybeArray $ createArray (sizeofArray a) (toAny e) (\ !_ -> pure ())

instance Applicative MaybeArray where
  pure :: a -> MaybeArray a
  pure a = MaybeArray $ runArray $ newArray 1 (toAny a)
  (<*>) :: MaybeArray (a -> b) -> MaybeArray a -> MaybeArray b
  MaybeArray ab <*> MaybeArray a = MaybeArray $ createArray (szab * sza) (error "impossible") $ \mb ->
    let go1 i = when (i < szab) $
          do
            f <- indexArrayM ab i
            go2 (i * sza) f 0
            go1 (i + 1)
        go2 off f j = when (j < sza) $
          do
            x <- indexArrayM a j
            let writeVal = toAny $ (anyToFunctor f :: Maybe a -> Maybe b) (unsafeToMaybe x) 
            writeArray mb (off + j) writeVal
            go2 off f (j + 1)
    in go1 0
      where szab = sizeofArray ab; sza = sizeofArray a
  MaybeArray a *> MaybeArray b = MaybeArray $ createArray (sza * szb) (error "impossible") $ \mb ->
    let go i | i < sza = copyArray mb (i * szb) b 0 szb
             | otherwise = return ()
    in go 0
      where sza = sizeofArray a; szb = sizeofArray b
  MaybeArray a <* MaybeArray b = MaybeArray $ createArray (sza*szb) (error "impossible") $ \ma ->
    let fill off i e | i < szb   = writeArray ma (off+i) e >> fill off (i+1) e
                     | otherwise = return ()
        go i | i < sza
             = do x <- indexArrayM a i
                  fill (i*szb) 0 x >> go (i+1)
             | otherwise = return ()
     in go 0
   where sza = sizeofArray a ; szb = sizeofArray b

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
