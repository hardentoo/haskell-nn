{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module Util where

import Types
import Data.Reflection (Reifies)
import Numeric.AD.Internal.Reverse (Reverse(Lift), Reverse, Tape)
import Numeric.AD


linlin :: (Fractional a) => (a, a) -> (a, a) -> a -> a
linlin (a, b) (c, d) x = (x - a)/(b - a) * (d - c)

linexp :: (Floating a) => (a, a) -> (a, a) -> a -> a
linexp (a, b) (c, d) x = exp (m * log d + (1 - m) *  log c)
  where
    m = (x - a)/(b - a)

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
msl f yss xss = (/2) $  sum $ zipWith (squaredLoss f) yss xss
  where
    -- | Per input cost
    squaredLoss :: (Floating a)
                   => Action a
                   -> Output a
                   -> Inputs a -> a
    squaredLoss f y xs = (f xs - y)**2



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
           -> [Output a]
           -> (forall a. (Floating a) =>  Loss a)
           -> (forall a. (Floating a) => a)
           -> [Inputs a]
           -> model a -> (a, model a)

descend toAction truth lossfunc γ xss model
  = gd γ (\m -> lossfunc (toAction m) (Lift <$> truth) $ fmap (fmap Lift) xss) model
