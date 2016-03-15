{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Util where

import Types
import Data.Reflection (Reifies)
import Numeric.AD.Internal.Reverse (Reverse, Tape)
import Numeric.AD.Internal.Reverse (Reverse)
import Numeric.AD

sigmoid :: (Floating a) => a -> a
sigmoid t = recip (1 + exp (negate t))

sigmoid' :: (Floating a) => a -> a
sigmoid' t = sigmoid t * (1 - sigmoid t)


dot :: (Floating a) => [a] -> [a] -> a
dot as bs
  | length as == length bs = sum $ zipWith (*) as bs
  | otherwise              = error "lengths don't match"


-- | Evaluate cost on a dataset
msl :: (Floating a) => Loss a
msl f truth = (/2) .  foldr ((+) . squaredLoss f truth) 0
  where
    -- | Per input cost
    squaredLoss :: (Floating a)
                   => Action a
                   -> IdealAction a
                   -> Inputs a -> a
    squaredLoss f truth xs = (f xs - truth xs)**2

gd :: (Floating a, Traversable model) =>
      (forall s. Reifies s Tape => model (Reverse s a) -> Reverse s a)
      -> model a -> model a
gd fn input = snd $ gradWith' (\x x' -> x - 0.1 * x') fn input
