{-# LANGUAGE ViewPatterns #-}

module Mandelbrot where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Data.Complex as A
import qualified Prelude as P

import Divergence
import FractalColour
import Fractal

mandelbrotFractal = Fractal 100 (\z c -> z*z + c) (\(unlift -> x :+ y) -> x * x + y * y <= 4) standartGradient