{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

#if __GLASGOW_HASKELL__ >= 805
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeInType #-}
#endif

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

main :: IO ()
main = do
  defaultMain $ testGroup "properties"
    [ testGroup "MaybeArray"
      [
        --lawsToTest (QCC.eqLaws (Proxy :: Proxy (MaybeArray Int)))
      --, lawsToTest (QCC.ordLaws (Proxy :: Proxy (MaybeArray Int)))
      --, lawsToTest (QCC.monoidLaws (Proxy :: Proxy (MaybeArray Int)))
      --, lawsToTest (QCC.showReadLaws (Proxy :: Proxy (MaybeArray Int)))
        lawsToTest (QCC.functorLaws (Proxy :: Proxy MaybeArray))
      , lawsToTest (QCC.applicativeLaws (Proxy :: Proxy MaybeArray))
      --, lawsToTest (QCC.monadLaws (Proxy :: Proxy MaybeArray))
      --, lawsToTest (QCC.foldableLaws (Proxy :: Proxy MaybeArray))
      --, lawsToTest (QCC.traversableLaws (Proxy :: Proxy MaybeArray))
      --, lawsToTest (QCC.isListLaws (Proxy :: Proxy (MaybeArray Int)))
      ]
    , testGroup "SmallMaybeArray"
      [
      --lawsToTest (QCC.eqLaws (Proxy :: Proxy (SmallMaybeArray Int)))
      --, lawsToTest (QCC.ordLaws (Proxy :: Proxy (SmallMaybeArray Int)))
      --, lawsToTest (QCC.monoidLaws (Proxy :: Proxy (SmallMaybeArray Int)))
      --, lawsToTest (QCC.showReadLaws (Proxy :: Proxy (MaybeArray Int)))
        lawsToTest (QCC.functorLaws (Proxy :: Proxy SmallMaybeArray))
      , lawsToTest (QCC.applicativeLaws (Proxy :: Proxy SmallMaybeArray))
      --, lawsToTest (QCC.monadLaws (Proxy :: Proxy SmallMaybeArray))
      --, lawsToTest (QCC.foldableLaws (Proxy :: Proxy SmallMaybeArray))
      --, lawsToTest (QCC.traversableLaws (Proxy :: Proxy SmallMaybeArray))
      --, lawsToTest (QCC.isListLaws (Proxy :: Proxy (SmallMaybeArray Int)))
      ]
    ]

int16 :: Proxy Int16
int16 = Proxy

int32 :: Proxy Int32
int32 = Proxy

lawsToTest :: QCC.Laws -> TestTree
lawsToTest (QCC.Laws name pairs) = testGroup name (map (uncurry TQC.testProperty) pairs)

instance Arbitrary1 MaybeArray where
  liftArbitrary elemGen = fmap fromList (QC.liftArbitrary elemGen)

instance Arbitrary a => Arbitrary (MaybeArray a) where
  arbitrary = fmap fromList QC.arbitrary

instance Arbitrary1 SmallMaybeArray where
  liftArbitrary elemGen = fmap smallMaybeArrayFromList (QC.liftArbitrary elemGen)

instance Arbitrary a => Arbitrary (SmallMaybeArray a) where
  arbitrary = fmap smallMaybeArrayFromList QC.arbitrary

