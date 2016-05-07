{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}

module Perceptron where

import System.Random
import Util


data Perceptron a = Perceptron { pBias :: a
                               , pWeights :: Weights a
                               }
                    deriving (Show, Foldable, Traversable)

instance Model Perceptron where
  toAction (Perceptron bias ws) xs
    = sigmoid $ ws `dot` xs + bias


initPerceptron :: (Random a, Floating a) => StdGen -> Int -> Perceptron a
initPerceptron seed dimensions = Perceptron 0 $ take dimensions $ randomRs (-1, 1) seed
