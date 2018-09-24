{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

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

import Prelude hiding (zipWith)
import Control.Applicative (Alternative(..), liftA2)
import Control.Monad (when, MonadPlus(..))
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.ST (ST, runST)
import Control.Monad.Zip (MonadZip(..))
import Control.Monad.Primitive
import Data.Primitive.SmallArray
import Data.Primitive.UnliftedArray (PrimUnlifted)
import Data.Data (Data(..), DataType, mkDataType, Constr, mkConstr, Fixity(..), constrIndex)
import Data.Function (fix)
import Data.Functor.Classes
import Data.Foldable hiding (toList)
import Data.Maybe (maybe)
import qualified Data.Foldable as Foldable

import Data.Primitive.Maybe.Internal
import GHC.Exts (Any,reallyUnsafePtrEquality#, IsList(..), SmallMutableArray#)
import Text.ParserCombinators.ReadP
import Unsafe.Coerce (unsafeCoerce)

newtype SmallMaybeArray a = SmallMaybeArray (SmallArray Any)
  deriving (PrimUnlifted)
newtype SmallMutableMaybeArray s a = SmallMutableMaybeArray (SmallMutableArray s Any)
  deriving (PrimUnlifted)

type role SmallMaybeArray representational
type role SmallMutableMaybeArray nominal representational

infixl 1 ?
(?) :: (a -> b -> c) -> (b -> a -> c)
(?) = flip
{-# INLINE (?) #-}

instance Functor SmallMaybeArray where
  fmap f (SmallMaybeArray arr) = SmallMaybeArray $
    createSmallArray (sizeofSmallArray arr) nothingSurrogate $ \mb ->
      let go i
            | i == (sizeofSmallArray arr) = return ()
            | otherwise = do
                x <- indexSmallArrayM arr i
                case unsafeToMaybe x of
                  Nothing -> pure () 
                  Just val -> writeSmallArray mb i (toAny (f val))
                go (i + 1)
      in go 0
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
  
  abm@(SmallMaybeArray ab) <*> am@(SmallMaybeArray a) = SmallMaybeArray $ createSmallArray (szab * sza) nothingSurrogate $ \mb ->
    let go1 i = when (i < szab) $ do
          case indexSmallMaybeArray abm i of
            Nothing -> pure ()
            Just f -> go2 (i * sza) f 0
          go1 (i + 1)
        go2 off f j = when (j < sza) $ do
          case indexSmallMaybeArray am j of
            Nothing -> pure ()
            Just v -> writeSmallArray mb (off + j) (toAny (f v))
          go2 off f (j + 1)
    in go1 0
      where szab = sizeofSmallArray ab; sza = sizeofSmallArray a

instance Traversable SmallMaybeArray where
  traverse = traverseSmallArray

traverseSmallArray :: Applicative f
  => (a -> f b)
  -> SmallMaybeArray a
  -> f (SmallMaybeArray b)
traverseSmallArray f =  \ !(SmallMaybeArray ary) ->
  let
    !len = sizeofSmallArray ary
    go !ix
      | ix == len = pure $ STA $ \mary -> unsafeFreezeSmallArray (SmallMutableArray mary)
      | otherwise = let x = indexSmallArray ary ix
                    in case unsafeToMaybe x of
                      Nothing -> go (ix + 1)
                      Just v -> liftA2 (\b (STA m) -> STA $ \mary ->
                                          writeSmallArray (SmallMutableArray mary) ix (toAny b) >> m mary)
                                       (f v) (go (ix + 1))
  in if len == 0
       then pure mempty
       else SmallMaybeArray <$> runSTA len <$> go 0

newtype STA a = STA { _runSTA :: forall s. SmallMutableArray# s a -> ST s (SmallArray a) }

runSTA :: Int -> STA a -> SmallArray a
runSTA !sz = \(STA m) -> runST $ newArray_ sz >>= \ar -> m (msarray# ar)

msarray# :: SmallMutableArray s a -> SmallMutableArray# s a
msarray# (SmallMutableArray m) = m
{-# INLINE msarray# #-}

newArray_ :: Int -> ST s (SmallMutableArray s a)
newArray_ !n = newSmallArray n badTraverseValue

badTraverseValue :: a
badTraverseValue = error "traverse: bad indexing"

data ArrayStack a
  = PushArray !(SmallArray a) !(ArrayStack a)
  | EmptyStack

instance Monad SmallMaybeArray where
  return = pure
  (>>) = (*>)
  (SmallMaybeArray ary) >>= f = SmallMaybeArray $ collect 0 EmptyStack (la - 1)
    where
      la = sizeofSmallArray ary
      collect sz stk i
        | i < 0 = createSmallArray sz nothingSurrogate $ fill 0 stk
        | otherwise = let x = indexSmallArray ary i
                      in case unsafeToMaybe x of
                        Nothing -> collect sz stk (i - 1)
                        Just v -> let (SmallMaybeArray sb) = f v
                                      lsb = sizeofSmallArray sb
                                  in if lsb == 0
                                       then collect sz stk (i - 1)
                                       else collect (sz + lsb) (PushArray sb stk) (i - 1)
      fill _ EmptyStack _ = return ()
      fill off (PushArray sb sbs) smb
        | let lsb = sizeofSmallArray sb
        = copySmallArray smb off sb 0 lsb
            *> fill (off + lsb) sbs smb

instance Alternative SmallMaybeArray where
  empty = mempty
  (<|>) = (<>)
  some a | sizeofSmallMaybeArray a == 0 = mempty
         | otherwise = error "some: infinite arrays are not well defined"
  many a | sizeofSmallMaybeArray a == 0 = pure []
         | otherwise = error "many: infinite arrays are not well defined"

instance MonadPlus SmallMaybeArray where
  mzero = empty
  mplus = (<|>)

instance MonadFail SmallMaybeArray where
  fail _ = empty

zipWith :: (a -> b -> c) -> SmallMaybeArray a -> SmallMaybeArray b -> SmallMaybeArray c
zipWith f (SmallMaybeArray aa) (SmallMaybeArray ab) = SmallMaybeArray $
  createSmallArray mn nothingSurrogate $ \mc ->
    let go i
          | i < mn = do
              x <- indexSmallArrayM aa i
              y <- indexSmallArrayM ab i
              let x' = unsafeToMaybe x
                  y' = unsafeToMaybe y
              case x' of
                Nothing -> go (i + 1)
                Just va -> case y' of
                  Nothing -> go (i + 1)
                  Just vb -> writeSmallArray mc i (toAny $ f va vb) >> go (i + 1)
          | otherwise = return ()
    in go 0
  where mn = sizeofSmallArray aa `min` sizeofSmallArray ab

instance MonadZip SmallMaybeArray where
  mzip aa ab = zipWith (,) aa ab
  mzipWith f aa ab = zipWith f aa ab
  munzip :: forall a b. SmallMaybeArray (a, b) -> (SmallMaybeArray a, SmallMaybeArray b)
  munzip (SmallMaybeArray aab) = runST $ do
    let sz = sizeofSmallArray aab
    ma_ <- newSmallArray sz nothingSurrogate :: ST s (SmallMutableArray s Any)
    mb_ <- newSmallArray sz nothingSurrogate :: ST s (SmallMutableArray s Any)
    let go :: forall s. Int -> SmallMutableArray s Any -> SmallMutableArray s Any -> ST s ()
        go i ma mb = if i < sz
          then do
            tab <- indexSmallArrayM aab i
            let (a, b) = fromAny tab
                a' = unsafeToMaybe a
                b' = unsafeToMaybe b
            maybe (pure ()) (writeSmallArray ma i) a'
            maybe (pure ()) (writeSmallArray mb i) b'
            go (i + 1) ma mb
          else return ()

    go 0 ma_ mb_
    (ma1, ma2) <- (,) <$> unsafeFreezeSmallArray ma_ <*> unsafeFreezeSmallArray mb_
    return (unsafeCoerce ma1, unsafeCoerce ma2) :: ST s (SmallMaybeArray a, SmallMaybeArray b)

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

instance Semigroup (SmallMaybeArray a) where
  SmallMaybeArray a1 <> SmallMaybeArray a2 = SmallMaybeArray $
    createSmallArray (sza1 + sza2) nothingSurrogate $ \ma ->
      copySmallArray ma 0 a1 0 sza1 >> copySmallArray ma sza1 a2 0 sza2
    where
      sza1 = sizeofSmallArray a1; sza2 = sizeofSmallArray a2

instance Monoid (SmallMaybeArray a) where
  mempty = SmallMaybeArray emptySmallArray
  mappend = (<>)

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

smallMaybeArrayLiftReadsPrec :: (Int -> ReadS a) -> ReadS [a] -> Int -> ReadS (SmallMaybeArray a)
smallMaybeArrayLiftReadsPrec _ listReadsPrec p = readParen (p > 10) . readP_to_S $ do
  () <$ string "fromListN"
  skipSpaces
  n <- readS_to_P reads
  skipSpaces
  l <- readS_to_P listReadsPrec
  return $ smallMaybeArrayFromListN n l

instance Read1 SmallMaybeArray where
  liftReadsPrec = smallMaybeArrayLiftReadsPrec

instance Read a => Read (SmallMaybeArray a) where
  readsPrec = smallMaybeArrayLiftReadsPrec readsPrec readList

smallMaybeArrayDataType :: DataType
smallMaybeArrayDataType = mkDataType "Data.Primitive.Array.Maybe.SmallMaybeArray" [fromListConstr]

fromListConstr :: Constr
fromListConstr = mkConstr smallMaybeArrayDataType "fromList" [] Prefix

instance Data a => Data (SmallMaybeArray a) where
  toConstr _ = fromListConstr
  dataTypeOf _ = smallMaybeArrayDataType
  gunfold k z c = case constrIndex c of
    1 -> k (z fromList)
    _ -> error "gunfold"
  gfoldl f z m = z fromList `f` toList m

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
            0# -> go (ix + 1)
            _ -> True
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
