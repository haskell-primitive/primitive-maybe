{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

#if __GLASGOW_HASKELL__ >= 805
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeInType #-}
#endif

import qualified Data.Foldable as Foldable
import Data.Proxy (Proxy(..))
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif

import Control.Monad.Zip (MonadZip)
import Control.Monad (MonadPlus)
import Data.Primitive.Array.Maybe
import Data.Primitive.SmallArray.Maybe
import GHC.Exts (IsList(..))
import Data.Functor.Classes

import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.QuickCheck (Arbitrary,Arbitrary1,Gen)
import qualified Test.Tasty.QuickCheck as TQC
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Classes as QCC

main :: IO ()
main = do
  defaultMain $ testGroup "properties"
    [ testGroup "MaybeArray" $ lawsToTest <$> maybeArrayLaws
    , testGroup "SmallMaybeArray" $ lawsToTest <$> smallMaybeArrayLaws
    ]

makeArrayLaws :: forall (f :: * -> *) a.
     (Monad f, MonadPlus f, MonadZip f, Foldable f, Eq1 f, Ord1 f, Show1 f, Arbitrary1 f)
  => (Read (f a), Show (Item (f a)), Monoid (f a), Ord (f a), Arbitrary (f a), Show (f a))
  => (IsList (f a), Show (Item (f a)), Arbitrary (Item (f a)))
  => Proxy f
  -> Proxy (f a)
  -> [QCC.Laws]
makeArrayLaws pf pfa =
  [ QCC.eqLaws pfa
  , QCC.ordLaws pfa
  , QCC.monoidLaws pfa
  , QCC.showReadLaws pfa
  , QCC.isListLaws pfa
  , QCC.functorLaws pf
  , QCC.alternativeLaws pf
  , QCC.applicativeLaws pf
  , QCC.foldableLaws pf
  , QCC.monadLaws pf 
  , QCC.monadPlusLaws pf
  , QCC.monadZipLaws pf
  ]

maybeArrayLaws :: [QCC.Laws]
maybeArrayLaws = makeArrayLaws proxyM1 proxyM

smallMaybeArrayLaws :: [QCC.Laws]
smallMaybeArrayLaws = makeArrayLaws proxyS1 proxyS

proxyM :: Proxy (MaybeArray Int)
proxyM = Proxy

proxyM1 :: Proxy MaybeArray
proxyM1 = Proxy

proxyS :: Proxy (SmallMaybeArray Int)
proxyS = Proxy

proxyS1 :: Proxy SmallMaybeArray
proxyS1 = Proxy

lawsToTest :: QCC.Laws -> TestTree
lawsToTest (QCC.Laws name pairs) = testGroup name (map (uncurry TQC.testProperty) pairs)

instance Arbitrary1 MaybeArray where
  liftArbitrary :: forall a. Gen a -> Gen (MaybeArray a) 
  liftArbitrary elemGen = fmap fromList (QC.liftArbitrary elemGen :: Gen [a])
  liftShrink :: forall a. (a -> [a]) -> MaybeArray a -> [MaybeArray a]
  liftShrink shrf m = fmap maybeArrayFromList (fmap shrf (Foldable.toList m))

instance Arbitrary a => Arbitrary (MaybeArray a) where
  arbitrary = QC.arbitrary1
  shrink = QC.shrink1

instance Arbitrary1 SmallMaybeArray where
  liftArbitrary :: forall a. Gen a -> Gen (SmallMaybeArray a) 
  liftArbitrary elemGen = fmap fromList (QC.liftArbitrary elemGen :: Gen [a])
  liftShrink :: forall a. (a -> [a]) -> SmallMaybeArray a -> [SmallMaybeArray a]
  liftShrink shrf m = fmap smallMaybeArrayFromList (fmap shrf (Foldable.toList m))

instance Arbitrary a => Arbitrary (SmallMaybeArray a) where
  arbitrary = QC.arbitrary1
  shrink = QC.shrink1

