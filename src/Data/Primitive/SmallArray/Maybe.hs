{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}
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

import Control.Monad (when)
import Control.Monad.Primitive
import Data.Primitive.SmallArray
import Data.Function (fix)

import Data.Primitive.Maybe.Internal
import GHC.Exts (Any,reallyUnsafePtrEquality#)
import Unsafe.Coerce (unsafeCoerce)

newtype SmallMaybeArray a = SmallMaybeArray (SmallArray Any)
newtype SmallMutableMaybeArray s a = SmallMutableMaybeArray (SmallMutableArray s Any)

type role SmallMaybeArray representational
type role SmallMutableMaybeArray nominal representational

infixl 1 ?
(?) :: (a -> b -> c) -> (b -> a -> c)
(?) = flip
{-# INLINE (?) #-}

instance Functor SmallMaybeArray where
  fmap f (SmallMaybeArray sa) = SmallMaybeArray $ createSmallArray (length sa) (error "impossible") $ \smb ->
    fix ? 0 $ \go i ->
      when (i < length sa) $ do
        x <- indexSmallArrayM sa i
        case (unsafeToMaybe x :: Maybe a) of
          Nothing -> writeSmallArray smb i (toAny Nothing :: Any) >> go (i + 1)
          Just a -> writeSmallArray smb i (toAny (Just (f a))) >> go (i + 1)
  {-# INLINE fmap #-}
  x <$ SmallMaybeArray sa = SmallMaybeArray $ createSmallArray (length sa) (toAny x) (\ !_ -> pure ())

instance Applicative SmallMaybeArray where
  pure a = SmallMaybeArray $ createSmallArray 1 (toAny a) (\ !_ -> pure ())

  SmallMaybeArray sa *> SmallMaybeArray sb = SmallMaybeArray $ createSmallArray (la*lb) (error "impossible") $ \smb ->
    fix ? 0 $ \go i ->
      when (i < la) $
        copySmallArray smb 0 sb 0 lb *> go (i+1)
   where
   la = length sa ; lb = length sb
  
  SmallMaybeArray a <* SmallMaybeArray b = SmallMaybeArray $ createSmallArray (sza*szb) (error "impossible") $ \ma ->
    let fill off i e = when (i < szb) $
                         writeSmallArray ma (off+i) e >> fill off (i+1) e
        go i = when (i < sza) $ do
                 x <- indexSmallArrayM a i
                 fill (i*szb) 0 x
                 go (i+1)
     in go 0
   where sza = sizeofSmallArray a ; szb = sizeofSmallArray b
  
  SmallMaybeArray ab <*> SmallMaybeArray a = SmallMaybeArray $ createSmallArray (szab*sza) (error "impossible") $ \mb ->
    let go1 i = when (i < szab) $
            do
              f <- indexSmallArrayM ab i
              go2 (i*sza) f 0
              go1 (i+1)
        go2 off f j = when (j < sza) $
            do
              x <- indexSmallArrayM a j
              let writeVal = toAny $ (anyToFunctor f :: Maybe a -> Maybe b) (unsafeToMaybe x)
              writeSmallArray mb (off + j) writeVal
              go2 off f (j + 1)
    in go1 0
   where szab = sizeofSmallArray ab ; sza = sizeofSmallArray a
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
