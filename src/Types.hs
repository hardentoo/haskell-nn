{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Types where

import Data.Reflection (Reifies)
import Numeric.AD.Internal.Reverse (Reverse, Tape)
import Numeric.AD.Internal.Reverse (Reverse)

type Weights a = [a]
type Input a = [a]
type Output a = a

type Batch a = [a]

type Action a = (Input a → a)
type IdealAction a = (Input a → a)

type Loss a = Action a → [Output a] → [Input a] → a
