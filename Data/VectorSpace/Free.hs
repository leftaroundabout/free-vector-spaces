-- |
-- Module      : Data.VectorSpace.Free
-- Copyright   : (c) Justus Sagemüller 2016
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemueller $ geo.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE CPP               #-}

module Data.VectorSpace.Free (
                             -- * Supported types
                               V0
                             , V1
                             , V2
                             , V3
                             , V4
                             -- * The vector-space type classes
                             , AffineSpace(..), AdditiveGroup(..)
                             , VectorSpace(..), InnerSpace(..), HasBasis(..)
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

import Control.Lens

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
