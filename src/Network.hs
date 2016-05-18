{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE MonomorphismRestriction #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- MonomorphismRestriction is necessary, because there will be no sharing for polymorphic values

module Network where

import Perceptron
import Data.Reify
import Util

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import System.IO.Unsafe (unsafePerformIO)
import GHC.TypeLits
import qualified Debug.SimpleReflect as SR
import Data.Proxy
import Data.Monoid


-- TODO: Parallelize rewrite rules?

data Arr (m :: Nat) (n :: Nat) = (KnownNat m, KnownNat n)
                                 => FromFunc (forall a. (Show a, Floating a) => [a] -> [a])

                               | forall (p :: Nat). KnownNat p
                                 => Arr m p :>>> Arr p n

                               | forall (o :: Nat) (p :: Nat). (KnownNat o, KnownNat p)
                                 => Arr o p :+++ Arr (m - o) (n - p)



toFunc :: forall (m :: Nat) (n :: Nat).
          Arr m n -> (forall a. (Show a, Floating a) => [a] -> [a])
toFunc (FromFunc fn) = fn
toFunc (a :>>> b) = toFunc b . toFunc a
toFunc (a :+++ b) = \xs -> (toFunc a $ take (inDim a) xs)
                           <> (toFunc b $ drop (inDim a) xs)


inDim :: forall (m::Nat) (n::Nat). (KnownNat m) => Arr m n -> Int
inDim _ = fromInteger $ natVal (Proxy :: Proxy m)

outDim :: forall (m::Nat) (n::Nat). (KnownNat n) =>  Arr m n -> Int
outDim _ = fromInteger $ natVal (Proxy :: Proxy n)

instance (KnownNat m, KnownNat n) => Show (Arr m n) where
  show arr = showFunc . toFunc $ arr
    where
      -- showFunc fn' = show (check . fn' $ replicate (inDim arr) SR.x)
      showFunc fn' = show (fn' $ replicate (inDim arr) SR.x)
      -- check xs = check' (length xs)
      --   where
      --     check' l
      --       | l == outDim arr = xs
      --       | otherwise       = error $ "output dimension invalid! " <> show l <> " " <> show (outDim arr)


foobar :: Arr 2 2
foobar = FromFunc (\[x, y] -> [x + 0, y + 1])

nth :: forall (m :: Nat). (KnownNat m) => Int -> Arr m 1
nth i = FromFunc (\x -> [x !! i])


copy :: forall (n :: Nat). (KnownNat n) => Arr 1 n
copy = FromFunc (\[x] -> replicate reiN x)
  where
    reiN = fromInteger $ natVal (Proxy :: Proxy n)

ass :: Arr 3 3
ass = x :+++ y
  where
    x :: Arr 2 2
    x = FromFunc ((+2)<$>)

    y :: Arr 1 1
    y = FromFunc (sin<$>)

poop :: (KnownNat m) => Arr m m
poop = x :>>> x
  where
    x = FromFunc ((+2)<$>)

















type Id = Int


data Network model a = Start (model a)
                     | Node (model a) [Network model a]
                     deriving (Show, Functor, Traversable, Foldable)


data NetworkF model a b = StartF (model a)
                        | NodeF (model a) [b]
                        deriving (Show)


instance MuRef (Network model a) where
  type DeRef (Network model a) = NetworkF model a
  mapDeRef _ (Start m)       = pure $ StartF m
  mapDeRef f (Node m inputs) = NodeF m <$> traverse f inputs


-- instance (Model model) => Model (Network model) where
--   toAction (Start m) = toAction m
--   toAction (Node m children) = \input -> toAction m (($input) . toAction <$> children)

--   randomizeModel :: (Random a, Floating a) => StdGen -> Network model a -> Network model a
--   randomizeModel seed (Node m children) => Node (randomizeModel seed m) newChildren
--     where
--   randomizeModel seed (Start m) => Start (randomizeModel seed m)

newtype Network' model a = Network' (IntMap (NetworkF model a Unique))
                        deriving (Show)

toIntMap :: Network model a -> Network' model a
toIntMap network = unsafePerformIO $ do
  Graph nodes s <- reifyGraph network
  let refs = IntMap.fromList nodes
  return $ Network' refs

-- instance (Model model) => Model (Network' model) where
--   toAction network' = toAction' network' 1
--     where
--       toAction' :: (Floating a) => Network' model a -> Unique -> Action a
--       toAction' orig@(Network' im) k = case im IntMap.! k of
--         StartF m         -> toAction m
--         NodeF m children -> \input -> toAction m (($input) . toAction' orig <$> children)

foo :: Floating a => Network Perceptron a
foo = Node (Perceptron 0 [3, 4]) [x, x]
  where
    x = Start (Perceptron 0 [1, 2])
