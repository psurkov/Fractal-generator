{-# LANGUAGE FlexibleContexts #-}

module Geometry where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.ViewState
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Data.Complex as A
import qualified Prelude as P
import qualified Data.Complex as C

type FractalPoint = Complex Float

-- TODO: phantom types


width :: Int
width = 400

height :: Int
height = 400


blWorldByWidthHeight :: (Int, Int) -> Point
blWorldByWidthHeight (w, h) = (-P.fromIntegral w P./ 2.0, -P.fromIntegral h P./ 2.0)

urWorldByWidthHeight :: (Int, Int) -> Point
urWorldByWidthHeight (w, h) = (P.fromIntegral w P./ 2.0, P.fromIntegral h P./ 2.0)

blWorldInit :: Point
blWorldInit = blWorldByWidthHeight (width, height)

urWorldInit :: Point
urWorldInit = urWorldByWidthHeight (width, height)

blComplexInit :: FractalPoint
blComplexInit = (-1) :+ (-1)

urComplexInit :: FractalPoint
urComplexInit = (1) :+ (1)

convertWorldToComplex :: Point -> FractalPoint
convertWorldToComplex (x, y) = xComplex C.:+ yComplex
                                where xRel = x P./ P.fst urWorldInit
                                      yRel = y P./ P.snd urWorldInit
                                      xComplex = xRel P.* C.realPart urComplexInit 
                                      yComplex = yRel P.* C.imagPart urComplexInit

makeComplexGrid :: Int -> Int -> FractalPoint -> FractalPoint -> Acc (A.Vector FractalPoint)
makeComplexGrid w h bl ur = use $ A.fromList (Z:.w P.* h) $ do
    imag <- make1DComplexGrid h (C.imagPart bl) (C.imagPart ur)
    real <- make1DComplexGrid w (C.realPart bl) (C.realPart ur)
    P.return (real :+ imag)
        where 
            make1DComplexGrid n l r = (P.+l) . (P./ P.fromIntegral n) . (P.*(r P.- l)) . P.fromIntegral P.<$> [0..n - 1]