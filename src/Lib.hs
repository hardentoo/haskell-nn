{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Lib
       ( module Types
       , module Util
       , module Lib
       ) where

import Prelude.Unicode
import System.Random
import Numeric.AD
import Debug.SimpleReflect
import qualified Text.Show.Pretty as Pr
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



genGrid :: [Inputs Double]
genGrid = do
  x <- [0..10]
  y <- [0..10]
  return [x - 5, y - 5]

pp :: (Show a) => a -> IO ()
pp = putStrLn . Pr.ppShow


descendToNand :: [(Double, Perceptron Double)]
descendToNand = take 1000
  $ iterate (descend neuron ideal msl genGrid . snd)
  $ (0, start)
  where
    ideal = neuron $ Perceptron 3 [-2, -2]
    -- ideal [x, y]
    --   | x > 0 && y > 0 = 0
    --   | otherwise      = 1
    start = Perceptron 0 [0, 0]
