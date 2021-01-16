module FractalColor where

import Data.Word
import qualified Data.ByteString as B

type FractalColor = [Word8]

type ColorFunc = Int -> FractalColor

twoColors :: Int -> FractalColor
twoColors (-1) = [100, 0, 0, 255]
twoColors _  = [0, 100, 0, 255]

colorDivergence :: ColorFunc -> Int -> FractalColor
colorDivergence = ($)

packColorsToByteString :: [FractalColor] -> B.ByteString
packColorsToByteString = B.pack . concat