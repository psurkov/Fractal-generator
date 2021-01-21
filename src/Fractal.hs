module Fractal where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Data.Complex as A
import Data.Array.Accelerate.Data.Colour.RGB as A
import qualified Prelude as P

import Geometry
import FractalColour
import Divergence

data Fractal = Fractal (Exp Int) IterFunc Checker ColourFunc

fractalColourInPoint :: Fractal -> Exp FractalPoint -> Exp A.Colour
fractalColourInPoint (Fractal iters f c cf) point = colorDivergence cf iters $ 
                                                        (calcDivergenceInPoint iters f c) point

fractalColourInPoints :: Fractal -> Acc (Matrix FractalPoint) -> Acc (Matrix A.Colour)
fractalColourInPoints f points = A.map (fractalColourInPoint f) points

fractalColoursOnGrid :: Fractal -> Int -> Int -> FractalPoint -> FractalPoint -> Acc (Matrix A.Colour)
fractalColoursOnGrid f w h bl ur = fractalColourInPoints f $ makeComplexGrid w h bl ur