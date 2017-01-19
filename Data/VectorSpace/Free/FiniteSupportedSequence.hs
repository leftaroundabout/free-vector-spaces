-- |
-- Module      : Data.VectorSpace.Free.FiniteSupportedSequence
-- Copyright   : (c) Justus Sagemüller 2016
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemueller $ geo.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE CPP                     #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module Data.VectorSpace.Free.FiniteSupportedSequence (
                             FinSuppSeq (..)
                             ) where

import Data.AffineSpace
import Data.VectorSpace
import Data.Basis

import qualified Data.Foldable as Foldable

import qualified Data.Vector.Unboxed as UArr
import qualified Data.Vector.Generic.Mutable as MArr

import GHC.Exts (IsList(..))




-- | The space of finitely-supported sequences is an /infinite/-dimensional space.
--   An vector of length /l/ is here understood as an infinite sequence that begins
--   with /l/ nonzero values, and continues with infinite zeroes.
-- 
--   You may also consider this as the type that languages like Octave/Matlab
--   (as well as Haskell's <http://hackage.haskell.org/package/hmatrix/ hmatrix> library)
--   approximate with their “vectors”, with one important difference: there is
--   no such thing as a dimensional-mismatch error, since we consider all these vectors
--   as elements of the same infinite-dimensional space. Adding two different-size
--   vectors will simply zero-pad the shorter, and unlike in Matlab this behaviour extends
--   consequently to matrix multiplication etc. (defined in
--   <http://hackage.haskell.org/package/linearmap-category/ linearmap-category>)
-- 
--   Of course it /can/ make sense to constrain the dimension, but for this the
--   type system should be used, not runtime checks.
-- 
--   (This is the same
--   behaviour that the <http://hackage.haskell.org/package/linear/ linear> library
--   gives to the standard list and vector types, but the problem there is that it
--   can't use unboxed arrays as these are not functors, but unboxing is crucial for
--   performance.)
newtype FinSuppSeq n = FinSuppSeq { getFiniteSeq :: UArr.Vector n }


{-# INLINE liftU2FSS #-}
liftU2FSS :: UArr.Unbox n => (n -> n -> n) -> FinSuppSeq n -> FinSuppSeq n -> FinSuppSeq n
-- Adapted from:
-- http://hackage.haskell.org/package/linear-1.20.5/docs/src/Linear.Vector.html#line-200 
liftU2FSS f (FinSuppSeq u) (FinSuppSeq v) = FinSuppSeq $ case compare lu lv of
    LT | lu == 0   -> v
       | otherwise -> UArr.modify
           (\ w -> Foldable.forM_ [0..lu-1] $
                \i -> MArr.unsafeWrite w i $ f (UArr.unsafeIndex u i)
                                               (UArr.unsafeIndex v i)) v
    EQ -> UArr.zipWith f u v
    GT | lv == 0   -> u
       | otherwise -> UArr.modify
            (\ w -> Foldable.forM_ [0..lv-1] $
                \i -> MArr.unsafeWrite w i $ f (UArr.unsafeIndex u i)
                                               (UArr.unsafeIndex v i)) u
 where lu = UArr.length u
       lv = UArr.length v


instance (Num n, UArr.Unbox n) => AffineSpace (FinSuppSeq n) where
  type Diff (FinSuppSeq n) = FinSuppSeq n
  (.-.) = (^-^)
  (.+^) = (^+^)
  
instance (Num n, UArr.Unbox n) => AdditiveGroup (FinSuppSeq n) where
  zeroV = FinSuppSeq $ UArr.empty
  (^+^) = liftU2FSS (+)
  negateV (FinSuppSeq v) = FinSuppSeq $ UArr.map negate v
  
instance (Num n, UArr.Unbox n) => VectorSpace (FinSuppSeq n) where
  type Scalar (FinSuppSeq n) = n
  μ*^FinSuppSeq v = FinSuppSeq $ UArr.map (μ*) v
  
instance (Num n, AdditiveGroup n, UArr.Unbox n) => InnerSpace (FinSuppSeq n) where
  FinSuppSeq v<.>FinSuppSeq w = UArr.sum (UArr.zipWith (*) v w)

instance (Num n, UArr.Unbox n) => HasBasis (FinSuppSeq n) where
  type Basis (FinSuppSeq n) = Int
  basisValue i = FinSuppSeq $ UArr.replicate i 0 `UArr.snoc` 1
  decompose = zip [0..] . toList
  decompose' (FinSuppSeq v) i = maybe 0 id $ v UArr.!? i

instance UArr.Unbox n => IsList (FinSuppSeq n) where
  type Item (FinSuppSeq n) = n
  fromListN l = FinSuppSeq . fromListN l
  fromList = FinSuppSeq . fromList
  toList = toList . getFiniteSeq

instance (UArr.Unbox n, Show n) => Show (FinSuppSeq n) where
  show = ("fromList "++) . show . toList
