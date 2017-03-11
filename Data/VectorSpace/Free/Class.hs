-- |
-- Module      : Data.VectorSpace.Free.Class
-- Copyright   : (c) Justus Sagemüller 2016
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemueller $ geo.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE FlexibleContexts        #-}

module Data.VectorSpace.Free.Class ( FreeVectorSpace(..) ) where

import Data.VectorSpace

import Foreign.C.Types (CFloat, CDouble)


-- | Vector spaces that are spanned by a specific, canonical set of basis vectors.
class (VectorSpace v, Num (Scalar v)) => FreeVectorSpace v where
  -- | Element-wise multiplication, equivalent to Matlab's @.*@ operator or
  --   @'L.liftI2' (*)@.
  (^*^) :: v -> v -> v
  -- | Like a monomorphic 'fmap'. Only guaranteed to act on the nonzero entries;
  --   whether the function is also applied on zeroes is instance-specific.
  vmap :: (Scalar v -> Scalar v) -> v -> v

instance FreeVectorSpace Float where {(^*^) = (*); vmap = id}
instance FreeVectorSpace Double where {(^*^) = (*); vmap = id}
instance FreeVectorSpace Int where {(^*^) = (*); vmap = id}
instance FreeVectorSpace Integer where {(^*^) = (*); vmap = id}
instance FreeVectorSpace CFloat where {(^*^) = (*); vmap = id}
instance FreeVectorSpace CDouble where {(^*^) = (*); vmap = id}

instance (FreeVectorSpace v, FreeVectorSpace w, Scalar v ~ Scalar w)
               => FreeVectorSpace (v,w) where
  (v,w)^*^(v',w') = (v^*^v', w^*^w')
  vmap f (v,w) = (vmap f v, vmap f w)

