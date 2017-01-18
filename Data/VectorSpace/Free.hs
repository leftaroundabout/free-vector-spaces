-- |
-- Module      : Data.VectorSpace.Free
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

module Data.VectorSpace.Free (
                             -- * Supported types
                             -- ** Fixed low dimension
                             -- | These come from the <http://hackage.haskell.org/package/linear/ linear> package.
                               V0
                             , V1
                             , V2
                             , V3
                             , V4
                             -- ** Arbitrary dimension
                             , FinSuppSeq
                             -- * The vector-space type classes
                             -- ** General
                             -- | These come from the <http://hackage.haskell.org/package/vector-space/ vector-space> package.
                             , AffineSpace(..), AdditiveGroup(..)
                             , VectorSpace(..), InnerSpace(..), HasBasis(..)
                             -- ** Small
                             , OneDimensional(..)
                             -- ** Free
                             , FiniteFreeSpace(..)
                             ) where

import Data.AffineSpace
import Data.VectorSpace
import Data.Basis

import Data.MemoTrie

import qualified Linear as L
import Linear.V0
import Linear.V1
import Linear.V2
import Linear.V3
import Linear.V4
import qualified Linear.Affine as LA

import Control.Lens ((^.), FoldableWithIndex, ifoldr)

import qualified Data.Foldable as Foldable

import qualified Data.Vector.Unboxed as UArr
import qualified Data.Vector.Generic.Mutable as MArr

import Data.Ratio
import Foreign.C.Types (CFloat, CDouble)

import GHC.Exts (IsList(..))

vDecomp :: FoldableWithIndex (L.E v) v => v s -> [(L.E v, s)]
vDecomp = ifoldr (\b s l -> (b,s):l) []

#define portFinDV(v)                              \
instance Num s => AffineSpace (v s) where {        \
  type Diff (v s) = v s;                            \
  (.-.) = (L.^-^);                                   \
  (.+^) = (L.^+^) };                                  \
instance Num s => AdditiveGroup (v s) where {          \
  zeroV = L.zero;                                       \
  (^+^) = (L.^+^);                                       \
  negateV = L.negated };                                  \
instance Num s => VectorSpace (v s) where {                \
  type Scalar (v s) = s;                                    \
  (*^) = (L.*^) };                                           \
instance (Num s, AdditiveGroup s) => InnerSpace (v s) where { \
  (<.>) = L.dot };                                             \
instance (Num s, AdditiveGroup s) => HasBasis (v s) where {     \
  type Basis (v s) = L.E v;                                      \
  decompose = vDecomp;                                            \
  basisValue = L.unit . L.el;                                      \
  decompose' w (L.E le) = w^.le }

portFinDV(V0)
portFinDV(V1)
portFinDV(V2)
portFinDV(V3)
portFinDV(V4)

#define portFinDP(v)                                 \
instance Num s => AffineSpace (LA.Point v s) where {  \
  type Diff (LA.Point v s) = v s;                      \
  (.-.) = (LA..-.);                                     \
  (.+^) = (LA..+^) }

portFinDP(V0)
portFinDP(V1)
portFinDP(V2)
portFinDP(V3)
portFinDP(V4)


instance HasTrie (L.E V0) where
  newtype L.E V0 :->: a = V0T (V0 a)
  trie f = V0T V0
  untrie (V0T v) (L.E i) = v^.i
  enumerate (V0T V0) = []
instance HasTrie (L.E V1) where
  newtype L.E V1 :->: a = V1T (V1 a)
  trie f = V1T $ V1 (f L.ex)
  untrie (V1T v) (L.E i) = v^.i
  enumerate (V1T (V1 x)) = [(L.ex, x)]
instance HasTrie (L.E V2) where
  newtype L.E V2 :->: a = V2T (V2 a)
  trie f = V2T $ V2 (f L.ex) (f L.ey)
  untrie (V2T v) (L.E i) = v^.i
  enumerate (V2T (V2 x y)) = [(L.ex, x), (L.ey, y)]
instance HasTrie (L.E V3) where
  newtype L.E V3 :->: a = V3T (V3 a)
  trie f = V3T $ V3 (f L.ex) (f L.ey) (f L.ez)
  untrie (V3T v) (L.E i) = v^.i
  enumerate (V3T (V3 x y z)) = [(L.ex, x), (L.ey, y), (L.ez, z)]
instance HasTrie (L.E V4) where
  newtype L.E V4 :->: a = V4T (V4 a)
  trie f = V4T $ V4 (f L.ex) (f L.ey) (f L.ez) (f L.ew)
  untrie (V4T v) (L.E i) = v^.i
  enumerate (V4T (V4 x y z w)) = [(L.ex, x), (L.ey, y), (L.ez, z), (L.ew, w)]


infixr 7 ^/^, ^/!

class (VectorSpace v, Fractional (Scalar v)) => OneDimensional v where
  -- | Compare the (directed) length of two vectors.
  (^/^) :: v -> v -> Maybe (Scalar v)
  -- | Unsafe version of '^/^'.
  (^/!) :: v -> v -> Scalar v
  v^/!w = case v^/^w of
       Just μ  -> μ
       Nothing -> 1/0

instance OneDimensional Float where
  _^/^0 = Nothing
  x^/^y = Just $ x/y
  x^/!y = x/y
instance OneDimensional Double where
  _^/^0 = Nothing
  x^/^y = Just $ x/y
  x^/!y = x/y
instance OneDimensional CFloat where
  _^/^0 = Nothing
  x^/^y = Just $ x/y
  x^/!y = x/y
instance OneDimensional CDouble where
  _^/^0 = Nothing
  x^/^y = Just $ x/y
  x^/!y = x/y
instance Integral i => OneDimensional (Ratio i) where
  _^/^0 = Nothing
  x^/^y = Just $ x/y
  x^/!y = x/y
instance (Eq r, Fractional r) => OneDimensional (V1 r) where
  _^/^V1 0 = Nothing
  V1 x^/^V1 y = Just $ x/y
  V1 x^/!V1 y = x/y



class (VectorSpace v, Num (Scalar v)) => FiniteFreeSpace v where
  {-# MINIMAL freeDimension, toFullUnboxVect, unsafeFromFullUnboxVect #-}
  freeDimension :: Functor p => p v -> Int
  toFullUnboxVect :: UArr.Unbox (Scalar v) => v -> UArr.Vector (Scalar v)
  unsafeFromFullUnboxVect :: UArr.Unbox (Scalar v) => UArr.Vector (Scalar v) -> v
  fromUnboxVect :: UArr.Unbox (Scalar v) => UArr.Vector (Scalar v) -> v
  fromUnboxVect v = result
   where result = case UArr.length v of
           0        -> zeroV
           n | n<d  -> unsafeFromFullUnboxVect $ v UArr.++ UArr.replicate (d-n) 0
         d = freeDimension [result]


instance FiniteFreeSpace Int where
  freeDimension _ = 1
  toFullUnboxVect = UArr.singleton
  unsafeFromFullUnboxVect v = UArr.unsafeIndex v 0
instance FiniteFreeSpace Float where
  freeDimension _ = 1
  toFullUnboxVect = UArr.singleton
  unsafeFromFullUnboxVect v = UArr.unsafeIndex v 0
instance FiniteFreeSpace Double where
  freeDimension _ = 1
  toFullUnboxVect = UArr.singleton
  unsafeFromFullUnboxVect v = UArr.unsafeIndex v 0

instance (FiniteFreeSpace u, FiniteFreeSpace v, Scalar u ~ Scalar v)
      => FiniteFreeSpace (u,v) where
  freeDimension puv = freeDimension (fmap fst puv) + freeDimension (fmap snd puv)
  toFullUnboxVect (u,v) = toFullUnboxVect u UArr.++ toFullUnboxVect v
  unsafeFromFullUnboxVect uv = (u,v)
   where u = unsafeFromFullUnboxVect uv
         v = unsafeFromFullUnboxVect $ UArr.drop du uv
         du = freeDimension [u]

instance Num s => FiniteFreeSpace (V0 s) where
  freeDimension _ = 0
  toFullUnboxVect _ = UArr.empty
  unsafeFromFullUnboxVect _ = zeroV
instance Num s => FiniteFreeSpace (V1 s) where
  freeDimension _ = 1
  toFullUnboxVect (V1 n) = UArr.singleton n
  unsafeFromFullUnboxVect v = V1 $ UArr.unsafeIndex v 0
instance Num s => FiniteFreeSpace (V2 s) where
  freeDimension _ = 2
  toFullUnboxVect (V2 x y) = UArr.fromListN 2 [x,y]
  unsafeFromFullUnboxVect v = V2 (UArr.unsafeIndex v 0)
                                 (UArr.unsafeIndex v 1)
instance Num s => FiniteFreeSpace (V3 s) where
  freeDimension _ = 3
  toFullUnboxVect (V3 x y z) = UArr.fromListN 3 [x,y,z]
  unsafeFromFullUnboxVect v = V3 (UArr.unsafeIndex v 0)
                                 (UArr.unsafeIndex v 1)
                                 (UArr.unsafeIndex v 2)
instance Num s => FiniteFreeSpace (V4 s) where
  freeDimension _ = 4
  toFullUnboxVect (V4 x y z w) = UArr.fromListN 3 [x,y,z,w]
  unsafeFromFullUnboxVect v = V4 (UArr.unsafeIndex v 0)
                                 (UArr.unsafeIndex v 1)
                                 (UArr.unsafeIndex v 2)
                                 (UArr.unsafeIndex v 3)




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
  (.-.) = liftU2FSS (-)
  (.+^) = liftU2FSS (+)
  
instance (Num n, UArr.Unbox n) => AdditiveGroup (FinSuppSeq n) where
  zeroV = FinSuppSeq $ UArr.empty
  (^-^) = liftU2FSS (-)
  (^+^) = liftU2FSS (+)
  negateV (FinSuppSeq v) = FinSuppSeq $ UArr.map negate v
  
instance (Num n, UArr.Unbox n) => VectorSpace (FinSuppSeq n) where
  type Scalar (FinSuppSeq n) = n
  μ*^FinSuppSeq v = FinSuppSeq $ UArr.map (μ*) v
  
instance (Num n, AdditiveGroup n, UArr.Unbox n) => InnerSpace (FinSuppSeq n) where
  FinSuppSeq v<.>FinSuppSeq w = UArr.sum (UArr.zipWith (*) v w)

instance UArr.Unbox n => IsList (FinSuppSeq n) where
  type Item (FinSuppSeq n) = n
  fromListN l = FinSuppSeq . fromListN l
  fromList = FinSuppSeq . fromList
  toList = toList . getFiniteSeq

instance (UArr.Unbox n, Show n) => Show (FinSuppSeq n) where
  show = ("fromList "++) . show . toList
