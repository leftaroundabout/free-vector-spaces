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
import Data.Ratio

import Foreign.C.Types (CFloat, CDouble, CSChar, CShort, CInt, CLong, CLLong, CIntMax)


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
instance FreeVectorSpace CSChar where {(^*^) = (*); vmap = id}
instance FreeVectorSpace CShort where {(^*^) = (*); vmap = id}
instance FreeVectorSpace CInt where {(^*^) = (*); vmap = id}
instance FreeVectorSpace CLong where {(^*^) = (*); vmap = id}
instance FreeVectorSpace CLLong where {(^*^) = (*); vmap = id}
instance FreeVectorSpace CFloat where {(^*^) = (*); vmap = id}
instance FreeVectorSpace CDouble where {(^*^) = (*); vmap = id}
instance FreeVectorSpace CIntMax where {(^*^) = (*); vmap = id}
instance Integral a => FreeVectorSpace (Ratio a) where {(^*^) = (*); vmap = id}

instance (FreeVectorSpace v, FreeVectorSpace w, Scalar v ~ Scalar w)
               => FreeVectorSpace (v,w) where
  (v,w)^*^(v',w') = (v^*^v', w^*^w')
  vmap f (v,w) = (vmap f v, vmap f w)
instance ( FreeVectorSpace u, FreeVectorSpace v, FreeVectorSpace w
         , Scalar v ~ Scalar u, Scalar v ~ Scalar w )
               => FreeVectorSpace (u,v,w) where
  (u,v,w)^*^(u',v',w') = (u^*^u', v^*^v', w^*^w')
  vmap f (u,v,w) = (vmap f u, vmap f v, vmap f w)
instance ( FreeVectorSpace u, FreeVectorSpace v, FreeVectorSpace w, FreeVectorSpace x
         , Scalar x ~ Scalar v, Scalar v ~ Scalar u, Scalar v ~ Scalar w )
               => FreeVectorSpace (u,v,w,x) where
  (u,v,w,x)^*^(u',v',w',x') = (u^*^u', v^*^v', w^*^w', x^*^x')
  vmap f (u,v,w,x) = (vmap f u, vmap f v, vmap f w, vmap f x)

