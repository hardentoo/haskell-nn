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
  x <- randomRs (-1, 1) $ mkStdGen 1337
  y <- randomRs (-1, 1) $ mkStdGen 1338
  return [x, y]

pp :: (Show a) => a -> IO ()
pp = putStrLn . Pr.ppShow


descendToNand :: [(Double, Perceptron Double)]
descendToNand = take 1000
  $ iterate (descend neuron ideal msl 0.1 (take 100 $ genGrid) . snd)
  $ (0, start)
  where
    ideal = neuron $ Perceptron 3 [-2, -2]
    start = Perceptron 0 [0, 0]


-- TODO: plot
