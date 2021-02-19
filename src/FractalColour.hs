{-# LANGUAGE ViewPatterns #-}
module FractalColour where

import Data.Word
import qualified Data.ByteString as B

import Graphics.Gloss as G
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Data.Complex as A
import Data.Array.Accelerate.Data.Colour.RGB as A
import Graphics.Gloss.Accelerate.Data.Picture as A
import qualified Prelude as P

import Data.Array.Accelerate.LLVM.Native as CPU

import Debug.Trace

type ColourFunc = Exp Int -> Exp Int -> Exp A.Colour

rgbInt :: Int -> Int -> Int -> Colour
rgbInt r g b = RGB (P.fromIntegral r / 255) (P.fromIntegral g / 255) (P.fromIntegral b / 255)

twoColours :: ColourFunc
twoColours x y = A.ifThenElse (y == constant (-1)) (rgb8 100 0 0) (rgb8 0 100 0)

standartGradient :: ColourFunc
standartGradient = gradient (constant 0.5) . use $ fromList (Z:.6) [(0, rgbInt 0 7 100), 
                                                     (0.16, rgbInt 32 107 203),
                                                     (0.42, rgbInt 237 255 255),
                                                     (0.6425, rgbInt 255 170 0),
                                                     (0.8575, rgbInt 0 2 0),
                                                     (1, rgbInt 0 7 100)]

redGlow :: ColourFunc
redGlow = gradient (constant 1.0) . use $ fromList (Z:.3) [(0, rgbInt 10 0 0), 
                                            (0.7, rgbInt 220 0 0),
                                            (1, rgbInt 220 100 0)]



gradient :: Exp Float -> Acc (A.Vector (Float, A.Colour)) -> ColourFunc
gradient recursiveFactor gradPoints n k = A.ifThenElse (k == constant (-1)) (rgb8 0 0 0) $ 
                            linearInterpolateColour (A.fst p1, A.fst p2) (A.snd p1, A.snd p2) smoothPercentRec --((A.fromIntegral k / A.fromIntegral n) A.^ (constant 3 :: Exp Int))
                            where
                                -- percent = A.fromIntegral k / A.fromIntegral n
                                smoothPercent = (A.logBase 2 (1 + A.fromIntegral k)) / (A.logBase 2 (1 + A.fromIntegral n))
                                smoothPercentRec = fst $ while (\(unlift->(i, rf)) -> i > rf && rf > constant 0.01)
                                        (\p -> lift (fst p - snd p, snd p * (1 - snd p)))
                                        (lift (smoothPercent, recursiveFactor))
                                ind = while (\i -> (A.fst $ (gradPoints !! i)) < smoothPercentRec)
                                        (\i -> i + constant 1)
                                        (constant 1)
                                p1 = gradPoints !! (ind - constant 1)
                                p2 = gradPoints !! ind
                                linearInterpolateColour :: (Exp Float, Exp Float) -> (Exp A.Colour, Exp A.Colour) -> Exp Float -> Exp A.Colour
                                linearInterpolateColour (x0, x1) (y0, y1) x = let
                                                                                  RGB r0 g0 b0 = unlift y0 :: RGB (Exp Float)
                                                                                  RGB r1 g1 b1 = unlift y1 :: RGB (Exp Float)
                                                                              in rgb (linearInterpolate (x0, x1) (r0, r1) x) (linearInterpolate (x0, x1) (g0, g1) x) (linearInterpolate (x0, x1) (b0, b1) x)
                                linearInterpolate (x0, x1) (y0, y1) x = y0 + (x - x0) * (y1 - y0) / (x1 - x0)


colorDivergence :: ColourFunc -> Exp Int -> Exp Int -> Exp A.Colour
colorDivergence = ($)

packColoursToPicture :: Acc (Matrix A.Colour) -> G.Picture
packColoursToPicture av = bitmapOfArray (CPU.run $ A.map packRGB av) True