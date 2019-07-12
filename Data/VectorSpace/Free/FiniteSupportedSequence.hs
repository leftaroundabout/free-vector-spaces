-- |
-- Module      : Data.VectorSpace.Free.FiniteSupportedSequence
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
{-# LANGUAGE CPP                     #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE MultiWayIf              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE UnicodeSyntax           #-}

module Data.VectorSpace.Free.FiniteSupportedSequence (
                               FinSuppSeq (..)
                             , SparseSuppSeq (..)
                             , SemisparseSuppSeq (..)
                             ) where

import Data.AffineSpace
import Data.VectorSpace
import Data.VectorSpace.Free.Class
import Data.Basis

import qualified Data.Foldable as Foldable

import qualified Data.Vector.Generic as Arr
import qualified Data.Vector.Unboxed as UArr
import qualified Data.Vector.Generic.Mutable as MArr

import GHC.Exts (IsList(..))

import Control.Arrow (first, second)
import Control.Monad (forM_)



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
  
instance (Num n, AdditiveGroup n, UArr.Unbox n) => FreeVectorSpace (FinSuppSeq n) where
  FinSuppSeq v^*^FinSuppSeq w = FinSuppSeq (UArr.zipWith (*) v w)
  vmap f (FinSuppSeq v) = FinSuppSeq $ UArr.map f v

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





-- | Sparsely supported sequences (what other languages would call /sparse vectors/)
--   are sequences consisting of lots of zeroes, with finitely many
--   nonzeroes scattered around. Only these nonzero elements are stored.
data SparseSuppSeq n = SparseSuppSeq {
       sparseNonzeroes :: UArr.Vector (Int,n)
     }

instance (Num n, UArr.Unbox n) => AffineSpace (SparseSuppSeq n) where
  type Diff (SparseSuppSeq n) = SparseSuppSeq n
  (.-.) = (^-^)
  (.+^) = (^+^)
  
instance (Num n, UArr.Unbox n) => AdditiveGroup (SparseSuppSeq n) where
  zeroV = SparseSuppSeq $ UArr.empty
  SparseSuppSeq u ^+^ SparseSuppSeq v = SparseSuppSeq w
   where w = Arr.unfoldrN (Arr.length u + Arr.length v) seekws (0,0)
         seekws (pu,pv) = case (u Arr.!? pu, v Arr.!? pv) of
                     (Just (ju,uj), Just (jv,vj))
                       -> if | ju>jv     -> Just ((jv, vj), (pu, pv+1))
                             | ju<jv     -> Just ((ju, uj), (pu+1, pv))
                             | otherwise -> Just ((ju, uj+vj), (pu+1, pv+1))
                     (Just (ju,uj), Nothing)
                                         -> Just ((ju, uj), (pu+1, pv))
                     (Nothing, Just (jv,vj))
                                         -> Just ((jv, vj), (pu, pv+1))
                     (Nothing, Nothing)  -> Nothing
  negateV (SparseSuppSeq v) = SparseSuppSeq $ Arr.map (second negate) v

instance (Num n, UArr.Unbox n) => VectorSpace (SparseSuppSeq n) where
  type Scalar (SparseSuppSeq n) = n
  μ *^ SparseSuppSeq v = SparseSuppSeq $ Arr.map (second (*μ)) v

instance (Num n, UArr.Unbox n) => FreeVectorSpace (SparseSuppSeq n) where
  SparseSuppSeq u ^*^ SparseSuppSeq v = SparseSuppSeq w
   where w = Arr.unfoldrN (Arr.length u `min` Arr.length v) seekws (0,0)
         seekws (pu,pv) = case (u Arr.!? pu, v Arr.!? pv) of
                     (Just (ju,uj), Just (jv,vj))
                       -> if | ju>jv     -> seekws (pu, pv+1)
                             | ju<jv     -> seekws (pu+1, pv)
                             | otherwise -> Just ((ju, uj*vj), (pu+1, pv+1))
                     _ -> Nothing
  vmap f (SparseSuppSeq v) = SparseSuppSeq $ Arr.map (second f) v

instance (Num n, AdditiveGroup n, UArr.Unbox n) => InnerSpace (SparseSuppSeq n) where
  v <.> w = case v ^*^ w of SparseSuppSeq vw -> Arr.foldl' (\acc (_,q) -> acc+q) 0 vw

instance (Num n, UArr.Unbox n) => HasBasis (SparseSuppSeq n) where
  type Basis (SparseSuppSeq n) = Int
  basisValue i = SparseSuppSeq $ UArr.singleton (i,1)
  decompose (SparseSuppSeq v) = UArr.toList v
  decompose' (SparseSuppSeq v) i = goBisect 0 (Arr.length v)
   where goBisect jb jt
           | jb==jt     = 0
           | otherwise  = case first (`compare`i) $ v Arr.! jm of
                            (LT,_) -> goBisect (jm+1) jt
                            (EQ,q) -> q
                            (GT,_) -> goBisect jb jm
          where jm = (jb+jt)`div`2

instance (UArr.Unbox n, Eq n, Num n) => IsList (SparseSuppSeq n) where
  type Item (SparseSuppSeq n) = n
  fromListN n xs = SparseSuppSeq $ Arr.unfoldrN n go (0,xs)
   where go (_,[]) = Nothing
         go (j,0:xs) = go (j+1,xs)
         go (j,x:xs) = Just ((j,x), (j+1,xs))
  fromList l = fromListN (length l) l
  toList (SparseSuppSeq xs) = go 0 0
   where go i j = case xs Arr.!? j of
              Just (i',x) | i==i'  -> x : go (i+1) (j+1)
              Nothing              -> []
              _                    -> 0 : go (i+1) j




-- | Like 'SparseSuppSeq', this type of number-sequence ignores zeroes and only stores
--   nonzero elements with positional information, but it does this not for every single
--   entry separately: only the first position of each contiguous /chunk/ of nonzeroes
--   is tracked. It is thus more suited for vectors that are in some places dense
--   but still have lots of zeroes.
-- 
--   The drawback is that random access (i.e. 'decompose'') has complexity 𝓞(𝑛)
--    – instead of 𝓞(1) for 'FinSuppSeq', or 𝓞(log 𝑛) for 'SparseSuppSeq' –
--   so this type should only be used for “abstract vector operations”.
data SemisparseSuppSeq n = SemisparseSuppSeq {
       chunkSparseNonzeroes :: UArr.Vector n
     , sparseNonzeroLocation :: UArr.Vector (Int, Int)
                                        -- ^ Start index of block,
                                        --        size of block of consecutive nonzeroes
     }
     

asSemisparse :: UArr.Unbox n => SparseSuppSeq n -> SemisparseSuppSeq n
asSemisparse (SparseSuppSeq v) = SemisparseSuppSeq (Arr.map snd v)
                                    $ Arr.unfoldrN (Arr.length v) mkIndex 0
 where mkIndex :: Int -> Maybe ((Int, Int), Int)
       mkIndex i
        | Just (j,_) <- v Arr.!? i  = case mkIndex $ i+1 of
            Just ((j',l),n) | j'==j+1  -> Just ((j,l+1), n)
            _                          -> Just ((j,1), i+1)
        | otherwise  = Nothing

fromSemisparse :: ∀ n . UArr.Unbox n => SemisparseSuppSeq n -> SparseSuppSeq n
fromSemisparse (SemisparseSuppSeq v ssIx) = SparseSuppSeq . (`Arr.zip`v) $ Arr.create (do
         ix <- MArr.new $ Arr.length v
         Arr.foldM_ (\i (j,l) -> do
                       forM_ [0..l-1] -- TODO: faster loop and unsafeWrite
                         $ \k -> MArr.write ix (i+k) (j+k)
                       return $ i+l
                    ) 0 ssIx
         return ix
     )

instance (Num n, UArr.Unbox n) => AffineSpace (SemisparseSuppSeq n) where
  type Diff (SemisparseSuppSeq n) = SemisparseSuppSeq n
  (.-.) = (^-^)
  (.+^) = (^+^)
  
instance (Num n, UArr.Unbox n) => AdditiveGroup (SemisparseSuppSeq n) where
  zeroV = SemisparseSuppSeq UArr.empty UArr.empty
  u ^+^ v =  -- TODO: faster, direct implementation
     asSemisparse $ fromSemisparse u ^+^ fromSemisparse v
  negateV (SemisparseSuppSeq v vis) = SemisparseSuppSeq (Arr.map negate v) vis
  
instance (Num n, UArr.Unbox n) => VectorSpace (SemisparseSuppSeq n) where
  type Scalar (SemisparseSuppSeq n) = n
  μ *^ SemisparseSuppSeq v ix = SemisparseSuppSeq (Arr.map (μ*) v) ix

instance (Num n, UArr.Unbox n) => FreeVectorSpace (SemisparseSuppSeq n) where
  u ^*^ v =  -- TODO: faster, direct implementation
      asSemisparse $ fromSemisparse u ^*^ fromSemisparse v
  vmap f (SemisparseSuppSeq v ix) = SemisparseSuppSeq (Arr.map f v) ix

instance (Num n, AdditiveGroup n, UArr.Unbox n) => InnerSpace (SemisparseSuppSeq n) where
  v <.> w = fromSemisparse v <.> fromSemisparse w

instance (Num n, UArr.Unbox n) => HasBasis (SemisparseSuppSeq n) where
  type Basis (SemisparseSuppSeq n) = Int
  basisValue i = SemisparseSuppSeq (UArr.singleton 1) (UArr.singleton (i,1))
  decompose = decompose . fromSemisparse
  decompose' v = decompose' $ fromSemisparse v

instance (UArr.Unbox n, Eq n, Num n) => IsList (SemisparseSuppSeq n) where
  type Item (SemisparseSuppSeq n) = n
  fromListN n = asSemisparse . fromListN n
  fromList = asSemisparse . fromList
  toList = toList . fromSemisparse
