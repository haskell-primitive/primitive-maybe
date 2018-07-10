{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

#if __GLASGOW_HASKELL__ >= 805
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeInType #-}
#endif

import qualified Data.Foldable as Foldable
import Data.Primitive
import Data.Proxy (Proxy(..))
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif

import Data.Primitive.Array.Maybe
import Data.Primitive.SmallArray.Maybe

import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.QuickCheck (Arbitrary,Arbitrary1,Gen)
import qualified Test.Tasty.QuickCheck as TQC
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Classes as QCC

main :: IO ()
main = do
  defaultMain $ testGroup "properties"
    [ testGroup "MaybeArray"
      [ lawsToTest (QCC.eqLaws (Proxy :: Proxy (MaybeArray Int)))
      , lawsToTest (QCC.ordLaws (Proxy :: Proxy (MaybeArray Int)))
      , lawsToTest (QCC.monoidLaws (Proxy :: Proxy (MaybeArray Int)))
      , lawsToTest (QCC.showReadLaws (Proxy :: Proxy (MaybeArray Int)))
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
      , lawsToTest (QCC.monoidLaws (Proxy :: Proxy (SmallMaybeArray Int)))
      , lawsToTest (QCC.showReadLaws (Proxy :: Proxy (MaybeArray Int)))
      , lawsToTest (QCC.functorLaws (Proxy :: Proxy SmallMaybeArray))
      , lawsToTest (QCC.applicativeLaws (Proxy :: Proxy SmallMaybeArray))
      --, lawsToTest (QCC.monadLaws (Proxy :: Proxy SmallMaybeArray))
      , lawsToTest (QCC.foldableLaws (Proxy :: Proxy SmallMaybeArray))
      --, lawsToTest (QCC.traversableLaws (Proxy :: Proxy SmallMaybeArray))
      , lawsToTest (QCC.isListLaws (Proxy :: Proxy (SmallMaybeArray Int)))
      ]
    ]

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

