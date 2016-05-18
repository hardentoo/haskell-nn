{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Lib where
       -- ( module Util
       -- , module Lib
       -- , module MLP
       -- ) where

import System.Random



import Perceptron
import Util
import Gen
import Network



nand :: (Floating a) => Perceptron a
nand = Perceptron 3 [-2, -2]


gradientDescentToNand :: [(Double, Perceptron Double)]
gradientDescentToNand = iterate step (0, start)
  where
    step :: (Double, Perceptron Double) -> (Double, Perceptron Double)
    step = descend (fmap ideal batch) meanSquareLoss 0.1 batch. snd

    batch = take 100 testRand
    ideal = toAction $ Perceptron 3 [-2, -2]
    start = randomizeModel (mkStdGen 1339) $ Perceptron 0 [0, 0]

-- | by the Law of Large Numbers, sampling with replacement (Binomial distribution) converges to sampling without replacement (Poisson distribution?)

stochasticGradientDescentToNand :: [(Double, Perceptron Double)]
stochasticGradientDescentToNand = map (\(score, model, _) -> (score, model))
  $ iterate step (0, start, batches)
  where
    step :: (Double, Perceptron Double, [BatchOf (Input Double)])
          -> (Double, Perceptron Double, [BatchOf (Input Double)])
    step (_,     _,                  []) = error "list should be infinite"
    step (_, model, batch:restOfBatches) = (score, nextModel, restOfBatches)
      where
        (score, nextModel) = descend (fmap ideal batch) meanSquareLoss 0.1 batch model

    batchSize = 100

    batches = testRandBatches batchSize

    ideal = toAction $ Perceptron 3 [-2, -2]
    start = randomizeModel (mkStdGen 1339) $ Perceptron 0 [0, 0]
