{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoMonoLocalBinds #-}

module Perceptron where

import System.Random
import Util


data Perceptron a = Perceptron { pBias :: a
                               , pWeights :: Weights a
                               }
                    deriving (Foldable, Traversable)
instance Show (Perceptron a) where
  show x = "{}"

instance Functor Perceptron where
  fmap f (Perceptron bias weights) = Perceptron (f bias) (f <$> weights)


instance Model Perceptron where
  toAction (Perceptron bias ws) xs
    = sigmoid $ ws `dot` xs + bias

  randomizeModel ∷ forall a. (Random a, Floating a) ⇒ StdGen → Perceptron a → Perceptron a
  randomizeModel seed (Perceptron bias ws) = Perceptron bias randomWeights
    where
      randomWeights = zipWith const ws (randomRs (-1, 1) seed :: [a])
