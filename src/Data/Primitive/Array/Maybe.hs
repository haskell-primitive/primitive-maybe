{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

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
  , maybeArrayFromList
  , maybeArrayFromListN
  , sizeofMaybeArray
  ) where

import Control.Monad (when)
import Control.Monad.Primitive
import Data.Primitive.Array
import Data.Foldable hiding (toList)
import Data.Functor.Classes
import qualified Data.Foldable as Foldable

import Data.Primitive.Maybe.Internal
import GHC.Exts (Any,reallyUnsafePtrEquality#, Int(..), IsList(..))
import Unsafe.Coerce (unsafeCoerce)

newtype MaybeArray a = MaybeArray (Array Any)
newtype MutableMaybeArray s a = MutableMaybeArray (MutableArray s Any)

type role MaybeArray representational
type role MutableMaybeArray nominal representational

instance Functor MaybeArray where
  fmap :: forall a b. (a -> b) -> MaybeArray a -> MaybeArray b
  fmap f (MaybeArray arr) = MaybeArray $
    createArray (sizeofArray arr) (error "impossible") $ \mb ->
      let go i
            | i == (sizeofArray arr) = return ()
            | otherwise = do
                x <- indexArrayM arr i
                case unsafeToMaybe x of
                  Nothing -> pure () 
                  Just val -> writeArray mb i (toAny (f val))
                go (i + 1)
      in go 0
  e <$ (MaybeArray a) = MaybeArray $ createArray (sizeofArray a) (toAny e) (\ !_ -> pure ())

instance Applicative MaybeArray where
  pure :: a -> MaybeArray a
  pure a = MaybeArray $ runArray $ newArray 1 (toAny a)
  (<*>) :: MaybeArray (a -> b) -> MaybeArray a -> MaybeArray b
  abm@(MaybeArray ab) <*> am@(MaybeArray a) = MaybeArray $ createArray (szab * sza) nothingSurrogate $ \mb ->
    let go1 i = when (i < szab) $ do
          case indexMaybeArray abm i of
            Nothing -> pure ()
            Just f -> go2 (i * sza) f 0
          go1 (i + 1)
        go2 off f j = when (j < sza) $ do
          case indexMaybeArray am j of
            Nothing -> pure ()
            Just v -> writeArray mb (off + j) (toAny (f v))
          go2 off f (j + 1)
    in go1 0
      where szab = sizeofArray ab; sza = sizeofArray a
  MaybeArray a *> MaybeArray b = MaybeArray $ createArray (sza * szb) nothingSurrogate $ \mb ->
    let go i | i < sza = copyArray mb (i * szb) b 0 szb
             | otherwise = return ()
    in go 0
      where sza = sizeofArray a; szb = sizeofArray b
  MaybeArray a <* MaybeArray b = MaybeArray $ createArray (sza*szb) nothingSurrogate $ \ma ->
    let fill off i e | i < szb   = writeArray ma (off+i) e >> fill off (i+1) e
                     | otherwise = return ()
        go i | i < sza
             = do x <- indexArrayM a i
                  fill (i*szb) 0 x >> go (i+1)
             | otherwise = return ()
     in go 0
   where sza = sizeofArray a ; szb = sizeofArray b

instance Foldable MaybeArray where
  -- Note: we perform the array lookups eagerly so we won't
  -- create thunks to perform lookups even if GHC can't see
  -- that the folding function is strict.
  foldr f = \z !(MaybeArray ary) ->
    let
      !sz = sizeofArray ary
      go i
        | i == sz = z
        | otherwise = let !x = indexArray ary i
                      in case unsafeToMaybe x of
                        Nothing -> z
                        Just val -> f val (go (i + 1))
    in go 0
  {-# INLINE foldr #-}
  foldl f = \z !(MaybeArray ary) ->
    let
      go i
        | i < 0 = z
        | otherwise = let !x = indexArray ary i
                      in case unsafeToMaybe x of
                        Nothing -> z
                        Just val -> f (go (i - 1)) val
    in go (sizeofArray ary - 1)
  {-# INLINE foldl #-}
  null (MaybeArray a) = sizeofArray a == 0
  {-# INLINE null #-}
  length (MaybeArray a) = sizeofArray a
  {-# INLINE length #-}
  sum = foldl' (+) 0
  {-# INLINE sum #-}
  product = foldl' (*) 1
  {-# INLINE product #-}

instance IsList (MaybeArray a) where
  type Item (MaybeArray a) = a
  fromListN = maybeArrayFromListN
  fromList  = maybeArrayFromList
  toList    = Foldable.toList

instance Eq a => Eq (MaybeArray a) where
  sma1 == sma2 = maybeArrayLiftEq (==) sma1 sma2

instance Eq1 MaybeArray where
  liftEq = maybeArrayLiftEq

instance Ord1 MaybeArray where
  liftCompare = maybeArrayLiftCompare

maybeArrayLiftEq :: (a -> b -> Bool) -> MaybeArray a -> MaybeArray b -> Bool
maybeArrayLiftEq p (MaybeArray sa1) (MaybeArray sa2) = length sa1 == length sa2 && loop (length sa1 - 1)
  where
    loop i
      | i < 0 = True
      | otherwise = let x = unsafeToMaybe (indexArray sa1 i)
                        y = unsafeToMaybe (indexArray sa2 i)
                    in case x of
                      Nothing -> case y of
                        Nothing -> True && loop (i - 1)
                        _       -> False
                      Just x' -> case y of
                        Nothing -> False
                        Just y' -> p x' y' && loop (i - 1)

maybeArrayLiftCompare :: (a -> b -> Ordering) -> MaybeArray a -> MaybeArray b -> Ordering
maybeArrayLiftCompare elemCompare (MaybeArray a1) (MaybeArray a2) = loop 0
  where
    la1 = length a1
    la2 = length a2
    mn = la1 `min` la2
    loop i
      | i < mn = let x = unsafeToMaybe (indexArray a1 i)
                     y = unsafeToMaybe (indexArray a2 i)
                 in case x of
                   Nothing -> case y of
                     Nothing -> EQ `mappend` loop (i + 1)
                     _       -> LT
                   Just x' -> case y of
                     Nothing -> GT
                     Just y' -> elemCompare x' y' `mappend` loop (i     + 1)
     | otherwise = compare la1 la2

instance Ord a => Ord (MaybeArray a) where
  compare sma1 sma2 = maybeArrayLiftCompare compare sma1 sma2

maybeArrayLiftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> MaybeArray a -> ShowS
maybeArrayLiftShowsPrec elemShowsPrec elemListShowsPrec p sa = showParen (p > 10) $
  showString "fromListN " . shows (length sa) . showString " "
  . listLiftShowsPrec elemShowsPrec elemListShowsPrec 11 (toList sa)

listLiftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> [a] -> ShowS
listLiftShowsPrec _ sl _ = sl

instance Show1 MaybeArray where
  liftShowsPrec = maybeArrayLiftShowsPrec

instance Show a => Show (MaybeArray a) where
  showsPrec p sa = maybeArrayLiftShowsPrec showsPrec showList p sa

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

maybeArrayFromListN :: Int -> [a] -> MaybeArray a
maybeArrayFromListN n l = MaybeArray $
  createArray n (error "uninitialized element") $ \sma ->
    let go !ix [] = if ix == n
          then return ()
          else error "list length less than specified size"
        go !ix (x : xs) = if ix < n
          then do
            writeArray sma ix (toAny x)
            go (ix+1) xs
          else error "list length greater than specified size"
    in go 0 l

maybeArrayFromList :: [a] -> MaybeArray a
maybeArrayFromList l = maybeArrayFromListN (length l) l

sizeofMaybeArray :: MaybeArray a -> Int
sizeofMaybeArray (MaybeArray a) = sizeofArray a
{-# INLINE sizeofMaybeArray #-}
