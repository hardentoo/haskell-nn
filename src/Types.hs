{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Types where

import Data.Reflection (Reifies)
import Numeric.AD.Internal.Reverse (Reverse, Tape)
import Numeric.AD.Internal.Reverse (Reverse)

type Weights a = [a]
type Inputs a = [a]

type Action a = (Inputs a -> a)
type IdealAction a = (Inputs a -> a)

type Loss a = Action a -> IdealAction a -> [Inputs a] -> a

-- | Given some actions and a loss function, return outputs given inputs
type Update model a = model a
                      -- -> (model a -> (forall s. Reifies s Tape =>  Action (Reverse s a)))
                      -> (model a -> Action a)
                      -> IdealAction a
                      -> Loss a
                      -> [Inputs a] -> model a
