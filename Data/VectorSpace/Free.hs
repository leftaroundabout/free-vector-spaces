-- |
-- Module      : Data.VectorSpace.Free
-- Copyright   : (c) Justus Sagemüller 2016
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemueller $ geo.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP          #-}

module Data.VectorSpace.Free ( module Linear.V0
                             , module Linear.V1
                             , module Linear.V2
                             , module Linear.V3
                             , module Linear.V4
                             , AffineSpace(..), AdditiveGroup(..)
                             , VectorSpace(..), InnerSpace(..) 
                             ) where

import Data.AffineSpace
import Data.VectorSpace

import qualified Linear as L
import Linear.V0
import Linear.V1
import Linear.V2
import Linear.V3
import Linear.V4


#define portFinDV(v)                         \
instance Num s => AffineSpace (v s) where {   \
  type Diff (v s) = v s;                       \
  (.-.) = (L.^-^);                              \
  (.+^) = (L.^+^) };                             \
instance Num s => AdditiveGroup (v s) where {     \
  zeroV = L.zero;                                  \
  (^+^) = (L.^+^);                                  \
  negateV = L.negated };                             \
instance Num s => VectorSpace (v s) where {           \
  type Scalar (v s) = s;                               \
  (*^) = (L.*^) }

portFinDV(V0)
portFinDV(V1)
portFinDV(V2)
portFinDV(V3)
portFinDV(V4)
