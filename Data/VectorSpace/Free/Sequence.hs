-- |
-- Module      : Data.VectorSpace.Free.Sequence
-- Copyright   : (c) Justus Sagemüller 2016
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsag $ hvl.no
-- Stability   : experimental
-- Portability : portable
-- 
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE DeriveFunctor           #-}

module Data.VectorSpace.Free.Sequence (
                             Sequence, BoxSequence, GSequence (..), minimumChunkSize
                             ) where

import Data.VectorSpace.Free.FiniteSupportedSequence

import Data.AffineSpace
import Data.VectorSpace
import Data.Basis

import qualified Data.Foldable as Foldable

import qualified Data.Vector as Arr
import qualified Data.Vector.Unboxed as UArr
import qualified Data.Vector.Generic as GArr
import qualified Data.Vector.Generic.Mutable as MArr

import GHC.Exts (IsList(..))




-- | The space of possibly-infinite sequences, isomorphic to the space of all lists
--   but implemented more efficiently (with exponentially-growing chunks of unboxed data,
--   so the overhead is amortised). In other words, this is a type of spine-lazy
--   but element-strict arrays.
-- 
--   This space is dual to 'FinSuppSeq', which is completely strict.
data GSequence array n
    = Sequence {
        sequenceHeads :: !(array n)
            -- ^ Length must be at most 'minimumChunkSize' in the outer constructor and
            --   double in each deeper layer. (Specification subject to future change!)
            --   If the length at depth 𝑑 is less than 2^𝑑, the remaining entries are
            --   treated as zeroes.
      , sequenceRemain :: GSequence array n
      }
    | SoloChunk {
        chunkOffset :: !Int
      , soloChunk :: !(array n)
            -- ^ Length plus offset must be at most 'minimumChunkSize' if this is
            --   the outer constructor and can double in each deeper layer.
      }
    

type Sequence = GSequence UArr.Vector
type BoxSequence = GSequence Arr.Vector

deriving instance Functor BoxSequence

reboxSequence :: (GArr.Vector a n, GArr.Vector b n) => GSequence a n -> GSequence b n
reboxSequence (SoloChunk o c) = SoloChunk o $ Arr.convert c
reboxSequence (Sequence h r) = Sequence (Arr.convert h) $ reboxSequence r

mapSequence :: (UArr.Unbox n, UArr.Unbox m) => (n -> m) -> Sequence n -> Sequence m
mapSequence f (SoloChunk i₀ chunk) = SoloChunk i₀ (UArr.map f chunk)
mapSequence f (Sequence hd rm) = Sequence (UArr.map f hd) (mapSequence f rm)

{-# INLINE liftU2Seq #-}
liftU2Seq :: UArr.Unbox n => n -> (n -> n -> n) -> Sequence n -> Sequence n -> Sequence n
liftU2Seq defaultVal f (Sequence hu ru) (Sequence hv rv)
  = (`Sequence`liftU2Seq defaultVal f ru rv) $ case compare lu lv of
-- Adapted from:
-- http://hackage.haskell.org/package/linear-1.20.5/docs/src/Linear.Vector.html#line-200 
    LT | lu == 0   -> hv
       | otherwise -> UArr.modify
           (\ w -> Foldable.forM_ [0..lu-1] $
                \i -> MArr.unsafeWrite w i $ f (UArr.unsafeIndex hu i)
                                               (UArr.unsafeIndex hv i)) hv
    EQ -> UArr.zipWith f hu hv
    GT | lv == 0   -> hu
       | otherwise -> UArr.modify
            (\ w -> Foldable.forM_ [0..lv-1] $
                \i -> MArr.unsafeWrite w i $ f (UArr.unsafeIndex hu i)
                                               (UArr.unsafeIndex hv i)) hu
 where lu = UArr.length hu
       lv = UArr.length hv
liftU2Seq defaultVal f (Sequence hu ru) (SoloChunk ov cv)
   | lu == 0, lv == 0  = Sequence UArr.empty ru
   | lu == 0      = Sequence (UArr.replicate ov defaultVal UArr.++ cv) ru
   | lv == 0      = Sequence hu ru
   | lu >= lv+ov  = Sequence (UArr.modify
                                (\w -> Foldable.forM_ [0..lv-1] $
                                   \i -> MArr.unsafeWrite w (i+ov)
                                            $ f (UArr.unsafeIndex hu (i+ov))
                                                (UArr.unsafeIndex cv i)) hu)
                             ru
   | ov == 0      = Sequence (UArr.modify
                                (\w -> Foldable.forM_ [0..lu-1] $
                                   \i -> MArr.unsafeWrite w i
                                            $ f (UArr.unsafeIndex hu i)
                                                (UArr.unsafeIndex cv i)) cv)
                             ru
   | otherwise    = Sequence (UArr.take ov hu UArr.++ UArr.modify
                                (\w -> Foldable.forM_ [ov..lu-1] $
                                   \i -> MArr.unsafeWrite w i
                                            $ f (UArr.unsafeIndex hu i)
                                                (UArr.unsafeIndex cv (i-ov))) cv)
                             ru
 where lu = UArr.length hu
       lv = UArr.length cv
liftU2Seq defaultVal f (SoloChunk ou cu) (Sequence hv rv)
   | lu == 0, lv == 0  = Sequence UArr.empty rv
   | lu == 0      = Sequence hv rv
   | lv == 0      = Sequence (UArr.replicate ou defaultVal UArr.++ cu) rv
   | lu+ou <= lv  = Sequence (UArr.modify
                                (\w -> Foldable.forM_ [0..lu-1] $
                                   \i -> MArr.unsafeWrite w (i+ou)
                                            $ f (UArr.unsafeIndex cu i)
                                                (UArr.unsafeIndex hv (i+ou))) hv)
                             rv
   | ou == 0      = Sequence (UArr.modify
                                (\w -> Foldable.forM_ [0..lv-1] $
                                   \i -> MArr.unsafeWrite w i
                                            $ f (UArr.unsafeIndex cu i)
                                                (UArr.unsafeIndex hv i)) cu)
                             rv
   | otherwise    = Sequence (UArr.take ou hv UArr.++ UArr.modify
                                (\w -> Foldable.forM_ [ou..lv-1] $
                                   \i -> MArr.unsafeWrite w i
                                            $ f (UArr.unsafeIndex cu (i-ou))
                                                (UArr.unsafeIndex hv i)) cu)
                             rv
 where lu = UArr.length cu
       lv = UArr.length hv
liftU2Seq _ f (SoloChunk ou cu) (SoloChunk ov cv)
   | lu == 0  = SoloChunk ov cv
   | lv == 0  = SoloChunk ou cu
   | ou >= ov, ou+lu <= ov+lv
        = SoloChunk ov $ UArr.modify
           (\ w -> Foldable.forM_ [0..lu-1] $
                \i -> MArr.unsafeWrite w i $ f (UArr.unsafeIndex cu i)
                                               (UArr.unsafeIndex cv (i+δo))) cv
   | ou <= ov, ou+lu >= ov+lv
        = SoloChunk ou $ UArr.modify
           (\ w -> Foldable.forM_ [0..lv-1] $
                \i -> MArr.unsafeWrite w i $ f (UArr.unsafeIndex cu (i-δo))
                                               (UArr.unsafeIndex cv i)) cu
   | ou >= ov
        = SoloChunk ov $ UArr.take δo cv UArr.++ UArr.modify
           (\ w -> Foldable.forM_ [δo..lv-1] $
                \i -> MArr.unsafeWrite w i $ f (UArr.unsafeIndex cu (i-δo))
                                               (UArr.unsafeIndex cv i)) cu
   | ou <= ov
        = SoloChunk ou $ UArr.take (-δo) cu UArr.++ UArr.modify
           (\ w -> Foldable.forM_ [-δo..lu-1] $
                \i -> MArr.unsafeWrite w i $ f (UArr.unsafeIndex cu i)
                                               (UArr.unsafeIndex cv (i+δo))) cv
 where lu = UArr.length cu
       lv = UArr.length cv
       δo = ou-ov


instance (Num n, UArr.Unbox n) => AffineSpace (Sequence n) where
  type Diff (Sequence n) = Sequence n
  (.-.) = (^-^)
  (.+^) = (^+^)
  
instance (Num n, UArr.Unbox n) => AdditiveGroup (Sequence n) where
  zeroV = SoloChunk 0 UArr.empty
  (^+^) = liftU2Seq 1 (+)
  negateV = mapSequence negate
  
instance (Num n, UArr.Unbox n) => VectorSpace (Sequence n) where
  type Scalar (Sequence n) = n
  μ*^v = mapSequence (μ*) v

instance (Num n, UArr.Unbox n) => HasBasis (Sequence n) where
  type Basis (Sequence n) = Int
  basisValue = bv minimumChunkSize
   where bv chunkSize i
          | i<chunkSize  = SoloChunk i (UArr.singleton 1)
          | otherwise    = Sequence (UArr.empty) $ bv (chunkSize*2) (i-chunkSize)
  decompose = zip [0..] . toList
  decompose' = dc minimumChunkSize
   where dc _ (SoloChunk o v) i
           | ir < 0              = 0
           | ir < UArr.length v  = UArr.unsafeIndex v ir
           | otherwise           = 0
          where ir = i-o
         dc chunkSize (Sequence h r) i
           | i < chunkSize  = maybe 0 id $ h UArr.!? i
           | otherwise      = dc (chunkSize*2) r (i-chunkSize)

instance (UArr.Unbox n, Num n) => IsList (Sequence n) where
  type Item (Sequence n) = n
  fromListN = fln minimumChunkSize
   where fln chunkSize l ns
           | l>chunkSize  = let (h,r) = splitAt chunkSize ns
                            in Sequence (UArr.fromList h)
                                   $ fln (chunkSize*2) (l-chunkSize) r
           | otherwise  = SoloChunk 0 $ UArr.fromList ns
  fromList = fln minimumChunkSize
   where fln chunkSize ns
           | null r     = SoloChunk 0 $ UArr.fromList ns
           | otherwise  = Sequence (UArr.fromList h)
                               $ fln (chunkSize*2) r
          where (h,r) = splitAt chunkSize ns
  toList = tl minimumChunkSize
   where tl _ (SoloChunk o c) = replicate o 0 ++ toList c
         tl chunkSize (Sequence h r)
             = toList h ++ replicate (chunkSize - UArr.length h) 0
                  ++ tl (chunkSize*2) r

instance (UArr.Unbox n, Show n, Num n) => Show (Sequence n) where
  show = ("fromList "++) . show . toList



minimumChunkSize :: Int
minimumChunkSize = 1
