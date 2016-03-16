module Mnist where

-- | taken from https://github.com/mhwombat/backprop-example/blob/master/Mnist.hs

import Data.Word
import Data.Binary.Get
import qualified Data.List.Split as S
import qualified Data.ByteString.Lazy as BL

data Image = Image { iRows :: Int
                   , iColumns :: Int
                   , iPixels :: [Word8]
                   } deriving (Eq, Show)

deserializeLabels :: Get (Word32, Word32, [Word8])
deserializeLabels = do
  magicNumber <- getWord32be
  count <- getWord32be
  labelData <- getRemainingLazyByteString
  let labels = BL.unpack labelData
  return (magicNumber, count, labels)

readLabels :: FilePath -> IO [Int]
readLabels filename = do
  content <- BL.readFile filename
  let (_, _, labels) = runGet deserializeLabels content
  return (map fromIntegral labels)

deserializeHeader :: Get (Word32, Word32, Word32, Word32, [[Word8]])
deserializeHeader = do
  magicNumber <- getWord32be
  imageCount <- getWord32be
  r <- getWord32be
  c <- getWord32be
  packedData <- getRemainingLazyByteString
  let len = fromIntegral (r * c)
  let unpackedData = S.chunksOf len (BL.unpack packedData)
  return (magicNumber, imageCount, r, c, unpackedData)

readImages :: FilePath -> IO [Image]
readImages filename = do
  content  <- BL.readFile filename
  let (_, _, r, c, unpackedData) = runGet deserializeHeader content
  return (map (Image (fromIntegral r) (fromIntegral c)) unpackedData)
