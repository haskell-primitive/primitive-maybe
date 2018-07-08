{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

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
  , thawSmallMaybeArray
  , smallMaybeArrayFromList
  , smallMaybeArrayFromListN
  , sizeofSmallMaybeArray
  ) where

import Control.Monad (when)
import Control.Monad.Primitive
import Data.Primitive.SmallArray
import Data.Function (fix)
import Data.Functor.Classes
import Data.Foldable hiding (toList)
import qualified Data.Foldable as Foldable

import Data.Primitive.Maybe.Internal
import GHC.Exts (Any,reallyUnsafePtrEquality#, IsList(..))
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

instance Foldable SmallMaybeArray where
  -- Note: we perform the array lookups eagerly so we won't
  -- create thunks to perform lookups even if GHC can't see
  -- that the folding function is strict.
  foldr f = \z !(SmallMaybeArray ary) ->
    let
      !sz = sizeofSmallArray ary
      go i
        | i == sz = z
        | otherwise = let !x = indexSmallArray ary i
                      in case unsafeToMaybe x of
                        Nothing -> z
                        Just val -> f val (go (i + 1))
    in go 0
  {-# INLINE foldr #-}
  foldl f = \z !(SmallMaybeArray ary) ->
    let
      go i
        | i < 0 = z
        | otherwise = let !x = indexSmallArray ary i
                      in case unsafeToMaybe x of
                        Nothing -> z
                        Just val -> f (go (i - 1)) val
    in go (sizeofSmallArray ary - 1)
  {-# INLINE foldl #-}
  null (SmallMaybeArray a) = sizeofSmallArray a == 0
  {-# INLINE null #-}
  length (SmallMaybeArray a) = sizeofSmallArray a
  {-# INLINE length #-}
  sum = foldl' (+) 0
  {-# INLINE sum #-}
  product = foldl' (*) 1
  {-# INLINE product #-}

instance IsList (SmallMaybeArray a) where
  type Item (SmallMaybeArray a) = a
  fromListN = smallMaybeArrayFromListN
  fromList  = smallMaybeArrayFromList
  toList    = Foldable.toList

smallMaybeArrayLiftEq :: (a -> b -> Bool) -> SmallMaybeArray a -> SmallMaybeArray b -> Bool
smallMaybeArrayLiftEq p (SmallMaybeArray sa1) (SmallMaybeArray sa2) = length sa1 == length sa2 && loop (length sa1 - 1)
  where
    loop i
      | i < 0 = True
      | otherwise = let x = unsafeToMaybe (indexSmallArray sa1 i)
                        y = unsafeToMaybe (indexSmallArray sa2 i)
                    in case x of
                      Nothing -> case y of
                        Nothing -> True && loop (i - 1)
                        _       -> False
                      Just x' -> case y of
                        Nothing -> False
                        Just y' -> p x' y' && loop (i - 1)
                    
instance Eq a => Eq (SmallMaybeArray a) where
  sma1 == sma2 = smallMaybeArrayLiftEq (==) sma1 sma2

instance Eq1 SmallMaybeArray where
  liftEq = smallMaybeArrayLiftEq

smallMaybeArrayLiftCompare :: (a -> b -> Ordering) -> SmallMaybeArray a -> SmallMaybeArray b -> Ordering
smallMaybeArrayLiftCompare elemCompare (SmallMaybeArray a1) (SmallMaybeArray a2) = loop 0
  where
    la1 = length a1
    la2 = length a2
    mn = la1 `min` la2
    loop i
      | i < mn = let x = unsafeToMaybe (indexSmallArray a1 i)
                     y = unsafeToMaybe (indexSmallArray a2 i)
                 in case x of
                   Nothing -> case y of
                     Nothing -> EQ `mappend` loop (i + 1)
                     _       -> LT
                   Just x' -> case y of
                     Nothing -> GT
                     Just y' -> elemCompare x' y' `mappend` loop (i + 1)
     | otherwise = compare la1 la2

instance Ord a => Ord (SmallMaybeArray a) where
  compare sma1 sma2 = smallMaybeArrayLiftCompare compare sma1 sma2

instance Ord1 SmallMaybeArray where
  liftCompare = smallMaybeArrayLiftCompare

smallMaybeArrayLiftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> SmallMaybeArray a -> ShowS
smallMaybeArrayLiftShowsPrec elemShowsPrec elemListShowsPrec p sa = showParen (p > 10) $
  showString "fromListN " . shows (length sa) . showString " "
    . listLiftShowsPrec elemShowsPrec elemListShowsPrec 11 (toList sa)

listLiftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> [a] -> ShowS
listLiftShowsPrec _ sl _ = sl

instance Show1 SmallMaybeArray where
  liftShowsPrec = smallMaybeArrayLiftShowsPrec

instance Show a => Show (SmallMaybeArray a) where
  showsPrec p sa = smallMaybeArrayLiftShowsPrec showsPrec showList p sa

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

thawSmallMaybeArray
  :: PrimMonad m
  => SmallMaybeArray a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> m (SmallMutableMaybeArray (PrimState m) a)
thawSmallMaybeArray (SmallMaybeArray a) off len =
  fmap SmallMutableMaybeArray (thawSmallArray a off len)

smallMaybeArrayFromListN :: Int -> [a] -> SmallMaybeArray a
smallMaybeArrayFromListN n l = SmallMaybeArray $
  createSmallArray n
      (error "uninitialized element") $ \sma ->
  let go !ix [] = if ix == n
        then return ()
        else error "list length less than specified size"
      go !ix (x : xs) = if ix < n
        then do
          writeSmallArray sma ix (toAny x)
          go (ix+1) xs
        else error "list length greater than specified size"
  in go 0 l

smallMaybeArrayFromList :: [a] -> SmallMaybeArray a
smallMaybeArrayFromList l = smallMaybeArrayFromListN (length l) l

sizeofSmallMaybeArray :: SmallMaybeArray a -> Int
sizeofSmallMaybeArray (SmallMaybeArray a) = sizeofSmallArray a
{-# INLINE sizeofSmallMaybeArray #-}
