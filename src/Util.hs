{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module Util where

import Types
import Data.Reflection (Reifies)
import Numeric.AD.Internal.Reverse (Reverse(Lift), Reverse, Tape)
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



-- | Gradient descent
-- | outputs (loss :: a, next :: model a)
gd :: (Floating a, Ord a, Traversable model) =>
      (forall a. (Floating a) =>  a)
      -> (forall s. (Ord a, Reifies s Tape) => model (Reverse s a) -> Reverse s a)
      -> model a
      -> (a, model a)
gd γ fn input = gradWith' (\x x' -> x - γ * x') fn input



descend :: (Floating a, Ord a, Traversable model)
           => (forall a. (Floating a) => model a -> Action a)
           -> (forall a. (Ord a, Floating a) => IdealAction a)
           -> (forall a. (Floating a) =>  Loss a)
           -> (forall a. (Floating a) => a)
           -> [Inputs a]
           -> model a -> (a, model a)

descend toAction truth lossfunc γ xss model
  = gd γ (\m -> lossfunc (toAction m) truth $ fmap (fmap Lift) xss) model
