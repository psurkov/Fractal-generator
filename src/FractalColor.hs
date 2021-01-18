module FractalColor where

import Data.Word
import qualified Data.ByteString as B

type FractalColor = [Word8]

type ColorFunc = Int -> Int -> FractalColor

twoColors :: ColorFunc
twoColors _ (-1) = [100, 0, 0, 255]
twoColors _ _  = [0, 100, 0, 255]

gradientColor :: ColorFunc
gradientColor _ (-1) = [0, 0, 0, 255]
gradientColor iters x = gradient 0 iters [0, 0, 0, 255] [255, 255, 255, 255] x

gradient :: Int -> Int -> FractalColor -> FractalColor -> Int -> FractalColor
gradient l r a b x = zipWith (+) a $ fmap (\t -> fromIntegral $ round $ fromIntegral (fromIntegral t * x) / (fromIntegral (r-l))) $ zipWith (-) b a

colorDivergence :: ColorFunc -> Int -> Int -> FractalColor
colorDivergence = ($)

packColorsToByteString :: [FractalColor] -> B.ByteString
packColorsToByteString = B.pack . concat