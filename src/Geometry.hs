module Geometry where

import Data.Complex
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.ViewState

type FractalPoint = Complex Float

-- TODO: phantom types


width :: Int
width = 400

height :: Int
height = 400


blWorldByWidthHeight :: (Int, Int) -> Point
blWorldByWidthHeight (w, h) = (-fromIntegral w/2.0, -fromIntegral h/2.0)

urWorldByWidthHeight :: (Int, Int) -> Point
urWorldByWidthHeight (w, h) = (fromIntegral w/2.0, fromIntegral h/2.0)

blWorldInit :: Point
blWorldInit = blWorldByWidthHeight (width, height)

urWorldInit :: Point
urWorldInit = urWorldByWidthHeight (width, height)

blComplexInit :: FractalPoint
blComplexInit = (-1) :+ (-1)

urComplexInit :: FractalPoint
urComplexInit = (1) :+ (1)

convertWorldToComplex :: Point -> FractalPoint
convertWorldToComplex (x, y) = xComplex :+ yComplex
                                where xRel = x / fst urWorldInit
                                      yRel = y / snd urWorldInit
                                      xComplex = xRel * realPart urComplexInit 
                                      yComplex = yRel * imagPart urComplexInit

makeComplexGrid :: Int -> Int -> FractalPoint -> FractalPoint -> [FractalPoint]
makeComplexGrid w h bl ur = do
    imag <- make1DComplexGrid h (imagPart bl) (imagPart ur)
    real <- make1DComplexGrid w (realPart bl) (realPart ur)
    return (real :+ imag)
        where 
            make1DComplexGrid n l r = (+l) . (/ fromIntegral n) . (*(r - l)) . fromIntegral <$> [0..n - 1]
