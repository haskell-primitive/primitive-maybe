{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

#if __GLASGOW_HASKELL__ >= 805
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeInType #-}
#endif

import qualified Data.Foldable as Foldable
import Control.Monad
import Control.Monad.ST
import Data.Primitive
import Data.Word
import Data.Proxy (Proxy(..))
import GHC.Int
import GHC.IO
import Data.Function (on)
import Control.Applicative (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Ord (Down(..))
import Data.Semigroup (stimes)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import Foreign.Storable (Storable)

import Data.Primitive.Array.Maybe
import Data.Primitive.SmallArray.Maybe

import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.QuickCheck (Arbitrary,Arbitrary1,Gen,(===),CoArbitrary,Function)
import qualified Test.Tasty.QuickCheck as TQC
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Classes as QCC
import qualified Test.QuickCheck.Classes.IsList as QCCL
import qualified Data.List as L
import qualified GHC.Exts as GHCExts

main :: IO ()
main = do
  defaultMain $ testGroup "properties"
    [ testGroup "MaybeArray"
      [ lawsToTest (QCC.eqLaws (Proxy :: Proxy (MaybeArray Int)))
      , lawsToTest (QCC.ordLaws (Proxy :: Proxy (MaybeArray Int)))
      --, lawsToTest (QCC.monoidLaws (Proxy :: Proxy (MaybeArray Int)))
      --, lawsToTest (QCC.showReadLaws (Proxy :: Proxy (MaybeArray Int)))
      , lawsToTest (QCC.functorLaws (Proxy :: Proxy MaybeArray))
      , lawsToTest (QCC.applicativeLaws (Proxy :: Proxy MaybeArray))
      --, lawsToTest (QCC.monadLaws (Proxy :: Proxy MaybeArray))
      , lawsToTest (QCC.foldableLaws (Proxy :: Proxy MaybeArray))
      --, lawsToTest (QCC.traversableLaws (Proxy :: Proxy MaybeArray))
      , lawsToTest (QCC.isListLaws (Proxy :: Proxy (MaybeArray Int)))
      ]
    , testGroup "SmallMaybeArray"
      [ lawsToTest (QCC.eqLaws (Proxy :: Proxy (SmallMaybeArray Int)))
      , lawsToTest (QCC.ordLaws (Proxy :: Proxy (SmallMaybeArray Int)))
      --, lawsToTest (QCC.monoidLaws (Proxy :: Proxy (SmallMaybeArray Int)))
      --, lawsToTest (QCC.showReadLaws (Proxy :: Proxy (MaybeArray Int)))
      , lawsToTest (QCC.functorLaws (Proxy :: Proxy SmallMaybeArray))
      , lawsToTest (QCC.applicativeLaws (Proxy :: Proxy SmallMaybeArray))
      --, lawsToTest (QCC.monadLaws (Proxy :: Proxy SmallMaybeArray))
      , lawsToTest (QCC.foldableLaws (Proxy :: Proxy SmallMaybeArray))
      --, lawsToTest (QCC.traversableLaws (Proxy :: Proxy SmallMaybeArray))
      , lawsToTest (QCC.isListLaws (Proxy :: Proxy (SmallMaybeArray Int)))
      ]
    ]

int16 :: Proxy Int16
int16 = Proxy

int32 :: Proxy Int32
int32 = Proxy

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

