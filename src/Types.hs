{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Types where

import Data.Reflection (Reifies)
import Numeric.AD.Internal.Reverse (Reverse, Tape)
import Numeric.AD.Internal.Reverse (Reverse)

type Weights a = [a]
type Inputs a = [a]
type Output a = a

type Action a = (Inputs a -> a)
type IdealAction a = (Inputs a -> a)

type Loss a = Action a -> [Output a] -> [Inputs a] -> a
