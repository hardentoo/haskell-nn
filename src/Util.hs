{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UnicodeSyntax #-}


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
meanSquareLoss :: (Floating a) ⇒ Loss a
meanSquareLoss f yss xss = (/2) $  sum $ zipWith (squaredLoss f) yss xss
  where
    -- | Per input cost
    squaredLoss :: (Floating a)
                   => Action a
                   -> Output a
                   -> Input a -> a
    squaredLoss f y xs = (f xs - y)**2



-- | Gradient descent
-- | outputs (loss :: a, next :: model a)
gd :: (Floating a, Ord a, Traversable model)

      ⇒ (∀ b.
         (Floating a) ⇒  a)

      → (∀ s.
         (Ord a, Reifies s Tape) ⇒ model (Reverse s a) → Reverse s a)

      → model a

      → (a, model a)

gd γ fn input = gradWith' (\x x' → x - γ * x') fn input



descend :: (Floating a, Ord a, Traversable model)

           => (∀ b.
               (Floating b) ⇒ model b -> Action b) -- toAction

           -> [Output a]                           -- truth

           -> (∀ b.
               (Floating b) ⇒  Loss b)             -- lossFunc

           -> (∀ b.
               (Floating b) ⇒ b)                   -- γ (learning rate)

           -> [Input a]                            -- inputs

           -> model a -> (a, model a)              -- model

descend toAction truth lossfunc γ xss model
  = gd γ (\m -> lossfunc (toAction m) (Lift <$> truth) $ fmap (fmap Lift) xss) model


-- stochasticDescend :: (Floating a, Ord a, Traversable model)

--            => (∀ a.
--                (Floating a) ⇒ model a -> Action a)

--            -> [Output a]

--            -> (∀ a.
--                (Floating a) ⇒  Loss a)

--            -> (∀ a.
--                (Floating a) ⇒ a)

--            -> [[Input a]] -- Batches!

--            -> model a -> (a, model a)

-- stochasticDescend toAction truth lossfunc γ batches model
--   = gd γ (\m -> lossfunc (toAction m) (Lift <$> truth) $ fmap (fmap Lift) xss) model
