module Mandelbrot where

import Data.Complex

import Divergence
import FractalColor
import Fractal

mandelbrotFractal = Fractal 40 (\z c -> z*z + c) (\c -> magnitude c <= 2) twoColors