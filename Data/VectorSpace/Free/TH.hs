{-# LANGUAGE TemplateHaskell         #-}

module Data.VectorSpace.Free.TH where

import Data.AffineSpace
import Data.Basis
import Data.VectorSpace
import qualified Linear as L
import Control.Lens (Lens')
import           Language.Haskell.TH
import qualified Linear.Affine as LA

-- | Generate 'VectorSpace' style instances from representable-functor ones.
--   Intended mostly for use with the types 'L.V1' ... 'L.V4' etc..
-- 
--   This macro generates instances for the classes 'AffineSpace', 'AdditiveGroup',
--   'VectorSpace', 'InnerSpace' and 'Basis'.
portFinDV t = [d|
  instance Num s => AffineSpace ($v s) where      
    type Diff ($v s) = $v s                        
    (.-.) = (L.^-^)                               
    (.+^) = (L.^+^)

  instance Num s => AdditiveGroup ($v s) where
    zeroV = L.zero                                 
    (^+^) = (L.^+^)                                       
    negateV = L.negated

  instance Num s => VectorSpace ($v s) where             
    type Scalar ($v s) = s                                 
    (*^) = (L.*^)

  instance (Num s, AdditiveGroup s) => InnerSpace ($v s) where
    (<.>) = L.dot

  instance (Num s, AdditiveGroup s) => HasBasis ($v s) where
    type Basis ($v s) = L.E $v                                    
    decompose = vDecomp                                           
    basisValue x = L.unit (L.el x :: Lens' ($v s) s)                 
    decompose' w (L.E le) = w^.le
  |]
  where
    v = conT t

-- | Generate an 'AffineSpace' instance for the 'LA.Point' of a representable functor.
portFinDP t = [d|
  instance Num s => AffineSpace (LA.Point $v s) where
    type Diff (LA.Point $v s) = $v s
    (.-.) = (LA..-.)
    (.+^) = (LA..+^) 
    |]
      where  v = conT t
