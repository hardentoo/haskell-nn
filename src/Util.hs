{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Util where

import Data.Reflection (Reifies)
import Numeric.AD.Internal.Reverse (Reverse(Lift), Reverse, Tape)
import Numeric.AD
import qualified Text.Show.Pretty as Pr


type Vector a = [a]

-- | Weights are vectors
type Weights a = Vector a

-- | Inputs are vectors
type Input a = Vector a

-- | Outputs are scalars
type Output a = a
type IdealOutput a = a


type BatchOf a = [a]


-- | Actions are functions from input to output
type Action a = (Input a → a)
type IdealAction a = Action a

-- | LossFunctions are functions from an action, a batch of Ideal
type LossFunction a = Action a → BatchOf(IdealOutput a) → BatchOf(Input a) → Loss a
type Loss a = a


-- | Models have an action
class (Traversable m, Foldable m)
      ⇒ Model m where
  toAction ∷ (Floating a) ⇒ m a → Action a




pp :: (Show a) => a -> IO ()
pp = putStrLn . Pr.ppShow

linlin :: (Fractional a) => (a, a) -> (a, a) -> a -> a
linlin (a, b) (c, d) x = c * (1 - m) + d * m
  where
    m = (x - a)/(b - a)

linexp :: (Floating a) => (a, a) -> (a, a) -> a -> a
linexp (a, b) (c, d) x = exp ((1 - m) * log c + m * log d)
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
meanSquareLoss :: (Floating a) ⇒ LossFunction a
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

      ⇒ (∀ b. (Floating a)
         ⇒  a                                     -- γ
        )

      → (∀ s. (Ord a, Reifies s Tape)
         ⇒ model (Reverse s a) → Reverse s a      -- fn
        )

      → model a  → (a, model a)                   -- m → (loss, m')

gd γ fn m = gradWith' toNewWeight fn m
  where
    toNewWeight x dx = x - γ * dx



descend :: ∀a model. (Floating a, Ord a, Model model)
           => [Output a]                           -- truth

           -> (∀ b. (Floating b)
               ⇒  LossFunction b                   -- lossFunc
              )

           -> (∀ b. (Floating b)
               ⇒ b                                 -- γ (learning rate)
              )

           -> [Input a]                            -- inputs

           -> model a -> (Loss a, model a)         -- model

descend truth lossfunc γ inputs model
  = gd γ lossWRTmodel model
  where
    lossWRTmodel ∷ (Reifies s Tape)
                   ⇒ model (Reverse s (Output a))
                   -> Loss (Reverse s (Output a))
    lossWRTmodel m = lossfunc (toAction m) (Lift <$> truth) $ (Lift<$>) <$> inputs



-- stochasticDescend :: (Floating a, Ord a, Traversable model)

--            => (∀ a.
--                (Floating a) ⇒ model a -> Action a)

--            -> [Output a]

--            -> (∀ a.
--                (Floating a) ⇒  LossFunction a)

--            -> (∀ a.
--                (Floating a) ⇒ a)

--            -> [[Input a]] -- Batches!

--            -> model a -> (a, model a)

-- stochasticDescend toAction truth lossfunc γ batches model
--   = gd γ (\m -> lossfunc (toAction m) (Lift <$> truth) $ fmap (fmap Lift) xss) model
