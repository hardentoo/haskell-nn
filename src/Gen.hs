{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Gen where

import System.Random

import Util

testGrid :: BatchOf (Input Double)
testGrid = do
  x <- [0..10]
  y <- [0..10]
  return $ linlin (0, 10) (-1, 1) <$> [x, y]

testRand :: BatchOf (Input Double)
testRand = do
  let n = 1
  x <- randomRs (-1 * n, n) $ mkStdGen 1337
  y <- randomRs (-1 * n, n) $ mkStdGen 1338
  return [x, y]

testRandBatches :: Int -> [BatchOf (Input Double)]
testRandBatches batchSize = do
  seed <- [0..]

  return $ take batchSize $ do
    let n = 1
    x <- randomRs (-1 * n, n) $ mkStdGen seed
    y <- randomRs (-1 * n, n) $ mkStdGen seed
    return [x, y]
