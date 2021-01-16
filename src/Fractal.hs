module Fractal where

import Geometry
import FractalColor
import Divergence

data Fractal = Fractal Int IterFunc Checker ColorFunc

fractalColorInPoint :: Fractal -> FractalPoint -> FractalColor
fractalColorInPoint (Fractal iters f c cf) point = colorDivergence cf $ 
                                                        (calcDivergenceInPoint iters f c) point

fractalColorInPoints :: Fractal -> [FractalPoint] -> [FractalColor]
fractalColorInPoints f = fmap (fractalColorInPoint f)

fractalColorsOnGrid :: Fractal -> Int -> Int -> FractalPoint -> FractalPoint -> [FractalColor]
fractalColorsOnGrid f w h bl ur = fractalColorInPoints f $ makeComplexGrid w h bl ur