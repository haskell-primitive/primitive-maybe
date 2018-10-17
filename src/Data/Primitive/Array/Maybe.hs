{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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

import Prelude hiding (zipWith)
import Control.Applicative (Alternative(..), liftA2)
import Control.Monad (when, MonadPlus(..))
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.ST (ST, runST)
import Control.Monad.Zip (MonadZip(..))
import Control.Monad.Primitive
import Data.Primitive.Array
import Data.Primitive.UnliftedArray (PrimUnlifted)
import Data.Foldable hiding (toList)
import Data.Functor.Classes
import qualified Data.Foldable as Foldable
import Data.Maybe (maybe)

import Data.Data
  (Data(..), DataType, mkDataType, Constr, mkConstr, Fixity(..), constrIndex)
import Data.Primitive.Maybe.Internal
import GHC.Exts (Any,reallyUnsafePtrEquality#, Int(..), IsList(..), MutableArray#)
import Text.ParserCombinators.ReadP
import Unsafe.Coerce (unsafeCoerce)

-- | An immutable array of boxed values of type @'Maybe' a@.
newtype MaybeArray a = MaybeArray (Array Any)
  deriving (PrimUnlifted)
-- | A mutable array of boxed values of type @'Maybe' a@.
newtype MutableMaybeArray s a = MutableMaybeArray (MutableArray s Any)
  deriving (PrimUnlifted)

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

instance Traversable MaybeArray where
  traverse = traverseArray

traverseArray :: Applicative f
  => (a -> f b)
  -> MaybeArray a
  -> f (MaybeArray b)
traverseArray f =  \ !(MaybeArray ary) ->
  let
    !len = sizeofArray ary
    go !ix
      | ix == len = pure $ STA $ \mary -> unsafeFreezeArray (MutableArray mary)
      | otherwise = let x = indexArray ary ix
                    in case unsafeToMaybe x of
                      Nothing -> go (ix + 1)
                      Just v -> liftA2 (\b (STA m) -> STA $ \mary ->
                                          writeArray (MutableArray mary) ix (toAny b) >> m mary)
                                       (f v) (go (ix + 1))
  in if len == 0
       then pure mempty
       else MaybeArray <$> runSTA len <$> go 0

newtype STA a = STA { _runSTA :: forall s. MutableArray# s a -> ST s (Array a) }

runSTA :: Int -> STA a -> Array a
runSTA !sz = \(STA m) -> runST $ newArray_ sz >>= \ar -> m (marray# ar)

newArray_ :: Int -> ST s (MutableArray s a)
newArray_ !n = newArray n badTraverseValue

badTraverseValue :: a
badTraverseValue = error "traverse: bad indexing"

instance Alternative MaybeArray where
  empty = mempty
  (<|>) = (<>)
  some a | sizeofMaybeArray a == 0 = mempty
         | otherwise = error "some: infinite arrays are not well defined"
  many a | sizeofMaybeArray a == 0 = pure []
         | otherwise = error "many: infinite arrays are not well defined"

instance MonadPlus MaybeArray where
  mzero = empty
  mplus = (<|>)

instance MonadFail MaybeArray where
  fail _ = empty

zipWith :: (a -> b -> c) -> MaybeArray a -> MaybeArray b -> MaybeArray c
zipWith f (MaybeArray aa) (MaybeArray ab) = MaybeArray $
  createArray mn nothingSurrogate $ \mc ->
    let go i
          | i < mn = do
              x <- indexArrayM aa i
              y <- indexArrayM ab i
              let x' = unsafeToMaybe x
                  y' = unsafeToMaybe y
              case x' of
                Nothing -> go (i + 1)
                Just va -> case y' of
                  Nothing -> go (i + 1)
                  Just vb -> writeArray mc i (toAny $ f va vb) >> go (i + 1)
          | otherwise = return ()
    in go 0
  where mn = sizeofArray aa `min` sizeofArray ab

instance MonadZip MaybeArray where
  mzip aa ab = zipWith (,) aa ab
  mzipWith f aa ab = zipWith f aa ab
  munzip :: forall a b. MaybeArray (a, b) -> (MaybeArray a, MaybeArray b) 
  munzip (MaybeArray aab) = runST $ do
    let sz = sizeofArray aab
    ma_ <- newArray sz nothingSurrogate :: ST s (MutableArray s Any)
    mb_ <- newArray sz nothingSurrogate :: ST s (MutableArray s Any)
    let go :: forall s. Int -> MutableArray s Any -> MutableArray s Any -> ST s ()
        go i ma mb = if i < sz
          then do
            tab <- indexArrayM aab i
            let (a, b) = fromAny tab
                a' = unsafeToMaybe a
                b' = unsafeToMaybe b
            maybe (pure ()) (writeArray ma i) a'
            maybe (pure ()) (writeArray mb i) b'
            go (i + 1) ma mb
          else return ()
    
    go 0 ma_ mb_
    (ma1, ma2) <- (,) <$> unsafeFreezeArray ma_ <*> unsafeFreezeArray mb_
    return (unsafeCoerce ma1, unsafeCoerce ma2) :: ST s (MaybeArray a, MaybeArray b)

data ArrayStack a
  = PushArray !(Array a) !(ArrayStack a)
  | EmptyStack

instance Monad MaybeArray where
  return = pure
  (>>) = (*>)
  (MaybeArray ary) >>= f = MaybeArray $ collect 0 EmptyStack (la - 1)
    where
      la = sizeofArray ary
      collect sz stk i
        | i < 0 = createArray sz nothingSurrogate $ fill 0 stk
        | otherwise = let x = indexArray ary i
                      in case unsafeToMaybe x of
                        Nothing -> collect sz stk (i - 1)
                        Just v -> let (MaybeArray sb) = f v
                                      lsb = sizeofArray sb
                                  in if lsb == 0
                                       then collect sz stk (i - 1)
                                       else collect (sz + lsb) (PushArray sb stk) (i - 1)
      fill _ EmptyStack _ = return ()
      fill off (PushArray sb sbs) smb
        | let lsb = sizeofArray sb
        = copyArray smb off sb 0 lsb
            *> fill (off + lsb) sbs smb

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

instance Semigroup (MaybeArray a) where
  (<>) :: MaybeArray a -> MaybeArray a -> MaybeArray a
  MaybeArray a1 <> MaybeArray a2 = MaybeArray $
    createArray (sza1 + sza2) nothingSurrogate $ \ma ->
      copyArray ma 0 a1 0 sza1 >> copyArray ma sza1 a2 0 sza2
    where
      sza1 = sizeofArray a1; sza2 = sizeofArray a2

instance Monoid (MaybeArray a) where
  mempty = MaybeArray emptyArray
  mappend = (<>)

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

maybeArrayLiftReadsPrec :: (Int -> ReadS a) -> ReadS [a] -> Int -> ReadS (MaybeArray a)
maybeArrayLiftReadsPrec _ listReadsPrec p = readParen (p > 10) . readP_to_S $ do
  () <$ string "fromListN"
  skipSpaces
  n <- readS_to_P reads
  skipSpaces
  l <- readS_to_P listReadsPrec
  return $ maybeArrayFromListN n l

instance Read1 MaybeArray where
  liftReadsPrec = maybeArrayLiftReadsPrec

instance Read a => Read (MaybeArray a) where
  readsPrec = maybeArrayLiftReadsPrec readsPrec readList

maybeArrayDataType :: DataType
maybeArrayDataType = mkDataType "Data.Primitive.Array.Maybe.MaybeArray" [fromListConstr]

fromListConstr :: Constr
fromListConstr = mkConstr maybeArrayDataType "fromList" [] Prefix

instance Data a => Data (MaybeArray a) where
  toConstr _ = fromListConstr
  dataTypeOf _ = maybeArrayDataType
  gunfold k z c = case constrIndex c of
    1 -> k (z fromList)
    _ -> error "gunfold"
  gfoldl f z m = z fromList `f` toList m

-- | Create a new 'MutableMaybeArray' of the given size and initialize all elements
--   with the given 'Maybe' value.
newMaybeArray :: PrimMonad m => Int -> Maybe a -> m (MutableMaybeArray (PrimState m) a)
{-# INLINE newMaybeArray #-}
newMaybeArray i ma = case ma of
  Just a -> do
    x <- newArray i (unsafeCoerce a)
    return (MutableMaybeArray x)
  Nothing -> do
    x <- newArray i nothingSurrogate
    return (MutableMaybeArray x)

-- | Get the 'Maybe' value at the given index out of a 'MaybeArray'.
indexMaybeArray :: MaybeArray a -> Int -> Maybe a
{-# INLINE indexMaybeArray #-}
indexMaybeArray (MaybeArray a) ix =
  let (# v #) = indexArray## a ix
   in unsafeToMaybe v

-- | Get the 'Maybe' value at the given index out of a 'MutableMaybeArray'.
readMaybeArray :: PrimMonad m => MutableMaybeArray (PrimState m) a -> Int -> m (Maybe a)
{-# INLINE readMaybeArray #-}
readMaybeArray (MutableMaybeArray m) ix = do
  a <- readArray m ix
  return (unsafeToMaybe a)

-- | Write a 'Maybe' value to the given index of a 'MutableMaybeArray'.
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

-- | Returns @True@ if the 'MaybeArray' contains a @Nothing@ value.
hasNothing :: MaybeArray a -> Bool
hasNothing (MaybeArray a) = go 0 where
  go !ix = if ix < sizeofArray a
    then
      let (# v #) = indexArray## a ix
       in case reallyUnsafePtrEquality# v nothingSurrogate of
            0# -> go (ix + 1)
            _ -> True
    else False

-- | Convert a 'MutableMaybeArray' to an immutable one without copying.
--   The array should not be modified after the conversion.
unsafeFreezeMaybeArray :: PrimMonad m => MutableMaybeArray (PrimState m) a -> m (MaybeArray a)
{-# INLINE unsafeFreezeMaybeArray #-}
unsafeFreezeMaybeArray (MutableMaybeArray ma) = do
  a <- unsafeFreezeArray ma
  return (MaybeArray a)

-- | Create a 'MutablePrimArray' from a slice of an immutable array.
--   This operation makes a copy of the specified slice, so it is safe
--   to use the immutable array afterward.
thawMaybeArray
  :: PrimMonad m
  => MaybeArray a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> m (MutableMaybeArray (PrimState m) a)
thawMaybeArray (MaybeArray a) off len =
  fmap MutableMaybeArray (thawArray a off len)

-- | Given the length of a list and a list of @a@,
--   build a 'MaybeArray' from the values in the list.
--   If the given 'Int' does not match the length of
--   the list, this function calls 'error'.
--   You should prefer this to 'maybeArrayFromList' if
--   the length of the list has already been computed.
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

-- | Given a list of @a@, build a 'MaybeArray' from
--   the values in the list.
maybeArrayFromList :: [a] -> MaybeArray a
maybeArrayFromList l = maybeArrayFromListN (length l) l

-- | Yield the size of the 'MaybeArray'.
sizeofMaybeArray :: MaybeArray a -> Int
sizeofMaybeArray (MaybeArray a) = sizeofArray a
{-# INLINE sizeofMaybeArray #-}
