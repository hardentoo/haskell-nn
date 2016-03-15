{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE RankNTypes #-}

module Lib
       ( module Types
       , module Util
       , module Lib
       ) where

import Prelude.Unicode
import System.Random
import Numeric.AD
import Debug.SimpleReflect
-- import Test.QuickCheck.Arbitrary

import Data.Reflection (Reifies)
import Numeric.AD.Internal.Reverse (Reverse(Lift), Reverse, Tape)


import Types
import Util



data Perceptron a = Perceptron { pBias :: a
                               , pWeights :: Weights a
                               }
                    deriving (Show, Foldable, Traversable)

instance Functor Perceptron where
  fmap f (Perceptron b ws) = Perceptron (f b) (f <$> ws)





neuron :: (Floating a) => Perceptron a -> Action a
neuron (Perceptron bias ws) xs = sigmoid $ ws `dot` xs + bias


nand :: (Floating a) => Perceptron a
nand = Perceptron 3 [-2, -2]



-- update' :: (Floating a) => a -> a -> a -> a
-- update' γ old deriv = old - γ *  deriv


-- gradient m toAction ideal loss xss

inTermsOfModel :: Floating a => (forall a. Floating a => model a -> Action a)
       -> (forall a. Floating a => IdealAction a)
       -> (forall a. Floating a => Loss a)
       -> [Inputs a]
       -> (forall s. Reifies s Tape => model (Reverse s a) -> Reverse s a)
inTermsOfModel toAction truth lossfunc xss = \m -> lossfunc (toAction m) truth $ fmap (fmap Lift) xss

updateStep :: (Floating a, Traversable model)
               => (forall a. (Floating a) => model a -> Action a)
               -> (forall a. (Floating a) => IdealAction a)
               -> (forall a. (Floating a) =>  Loss a)
               -> [Inputs a]
               -> model a -> model a

updateStep toAction truth lossfunc xss model
  = gd (inTermsOfModel toAction truth lossfunc xss) model



genGrid :: [Inputs Double]
genGrid = do
  x <- [0..10]
  y <- [0..10]
  return [x - 5, y - 5]

dude = updateStep neuron (neuron nand) msl genGrid (Perceptron 0 [0, 0])
