module Divergence where

import Data.Complex
import Geometry

-- f(z, c)
type IterFunc = FractalPoint -> FractalPoint -> FractalPoint

type Checker = FractalPoint -> Bool

-- return n if diverges in n iterations, else -1
calcDivergenceInPoint :: Int -> IterFunc -> Checker -> FractalPoint -> Int
calcDivergenceInPoint iters func checker point = helper iters (flip func point) checker 0 0
    where 
        helper iters f checker point cur_iter | not (checker $ f point) = cur_iter
                                              | cur_iter < iters = helper iters f checker (f point) (cur_iter + 1)
                                              | otherwise = (-1)       

calcDivergenceInPoints :: Int -> IterFunc -> Checker -> [FractalPoint] -> [Int]
calcDivergenceInPoints iters f checker points = calcDivergenceInPoint iters f checker <$> points