{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Lib
       ( module Types
       , module Util
       , module Lib
       ) where

import Prelude.Unicode
import System.Random
import Numeric.AD
import Debug.SimpleReflect
import qualified Text.Show.Pretty as Pr
import Unsafe.Coerce

import Data.Reflection (Reifies)
import Numeric.AD.Internal.Reverse (Reverse(Lift), primal,  Reverse, Tape)


import Types
import Util
import Mnist


data Perceptron a = Perceptron { pBias :: a
                               , pWeights :: Weights a
                               }
                    deriving (Show, Foldable, Traversable)

instance Functor Perceptron where
  fmap f (Perceptron b ws) = Perceptron (f b) (f <$> ws)


initPerceptron :: (Random a, Floating a) => StdGen -> Int -> Perceptron a
initPerceptron seed dimensions = Perceptron 0 $ take dimensions $ randomRs (-1, 1) seed





neuron :: (Floating a) => Perceptron a -> Action a
neuron (Perceptron bias ws) xs = sigmoid $ ws `dot` xs + bias


nand :: (Floating a) => Perceptron a
nand = Perceptron 3 [-2, -2]


genGrid :: [Input Double]
genGrid = do
  x <- [0..10]
  y <- [0..10]
  return $ linlin (0, 10) (-1, 1) <$> [x, y]

genRand :: [Input Double]
genRand = do
  let n = 1
  x <- randomRs (-1 * n, n) $ mkStdGen 1337
  y <- randomRs (-1 * n, n) $ mkStdGen 1338
  return [x, y]

genRandBatches :: [[Input Double]]
genRandBatches = do
  let numBatches = 1000
  seed <- [0..numBatches]

  return $ take 100 $ do
    let n = 1
    x <- randomRs (-1 * n, n) $ mkStdGen seed
    y <- randomRs (-1 * n, n) $ mkStdGen seed
    return [x, y]

pp :: (Show a) => a -> IO ()
pp = putStrLn . Pr.ppShow


descendToNand :: [(Double, Perceptron Double)]
descendToNand = take iterations
  $ iterate (descend neuron (fmap ideal myData) meanSquareLoss 0.1 myData . snd)
  $ (0, start)
  where
    iterations = 1000
    myData = take 100 $ genRand
    ideal = neuron $ Perceptron 3 [-2, -2]
    start = initPerceptron (mkStdGen 1339) 2

stochasticDescendToNand :: [(Double, Perceptron Double)]
stochasticDescendToNand = take iterations $ map (\(score, model, _) -> (score, model))
  $ iterate fn (0, start, batches)
  where
    fn :: (Double, Perceptron Double, [Batch (Input Double)])
          -> (Double, Perceptron Double, [Batch (Input Double)])
    fn (_, model, []) = error "should not be empty"
    fn (_, model, batch:restOfBatches) =
      let
        (score, nextModel) = descend neuron (fmap ideal batch) meanSquareLoss 0.1 batch model
      in
        case restOfBatches of
          [] -> (score, nextModel, batches)
          otherwise -> (score, nextModel, restOfBatches)

    iterations = 1000
    batches = genRandBatches
    ideal = neuron $ Perceptron 3 [-2, -2]
    start = initPerceptron (mkStdGen 1339) 2
