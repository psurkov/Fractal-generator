{-# LANGUAGE ViewPatterns #-}
module FractalColour where

import Data.Word
import qualified Data.ByteString as B

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Data.Complex as A
import Data.Array.Accelerate.Data.Colour.RGB as A
import qualified Prelude as P

import Data.Array.Accelerate.LLVM.Native as CPU

import Debug.Trace

type ColourFunc = Exp Int -> Exp Int -> Exp A.Colour

twoColours :: ColourFunc
twoColours x y = A.ifThenElse (y == constant (-1)) (rgb8 100 0 0) (rgb8 0 100 0)

redGradientColour :: ColourFunc
redGradientColour = undefined
-- redGradientColour _ (-1) = [0, 0, 0, 255]
-- redGradientColour iters x = gradient 0 iters [0, 0, 0, 255] [200, 100, 100, 255] x

gradient :: Exp Int -> Exp Int -> Exp A.Colour -> Exp A.Colour -> Exp Int -> Exp A.Colour
gradient = undefined
-- gradient l r a b x = zipWith (+) a $ fmap (\t -> fromIntegral $ round $ fromIntegral (fromIntegral t * x) / (fromIntegral (r-l))) $ zipWith (-) b a

colorDivergence :: ColourFunc -> Exp Int -> Exp Int -> Exp A.Colour
colorDivergence = ($)

-- packColoursToByteString :: Acc (Vector A.Colour) -> B.ByteString
-- packColoursToByteString = undefined
-- packColoursToByteString = B.pack . P.concat . A.toList . CPU.run
-- packColoursToByteString = B.pack . P.concat

tt :: Vector A.Colour -> B.ByteString
tt x = B.pack $ P.concat $ P.fmap helper (A.toList x)
    where helper (RGB r g b) = P.fmap P.floor $ P.fmap (*255) [r, g, b, 1]

packColoursToByteString :: Acc (Vector A.Colour) -> B.ByteString
packColoursToByteString av = tt $ CPU.run av