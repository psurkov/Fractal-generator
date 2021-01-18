module Fractal where

import Data.Complex
import qualified Data.KdMap.Static as KDM

import Geometry
import FractalColor
import Divergence

import Debug.Trace

data Fractal = Fractal Int IterFunc Checker ColorFunc

fractalColorInPoint :: Fractal -> FractalPoint -> FractalColor
fractalColorInPoint (Fractal iters f c cf) point = colorDivergence cf $ 
                                                        (calcDivergenceInPoint iters f c) point

fractalColorInPoints :: Fractal -> [FractalPoint] -> [FractalColor]
fractalColorInPoints f = fmap (fractalColorInPoint f)

fractalColorsOnGrid :: Fractal -> Int -> Int -> FractalPoint -> FractalPoint -> [FractalColor]
fractalColorsOnGrid f w h bl ur = fractalColorInPoints f $ makeComplexGrid w h bl ur


-- KDT color calculation

nearestAcceptedDist :: Float
nearestAcceptedDist = 0.5

constructDistOnScreen :: Int -> Int -> FractalPoint -> FractalPoint -> (FractalPoint -> FractalPoint -> Float)
constructDistOnScreen w h (blRe :+ blIm) (urRe :+ urIm) (aRe :+ aIm) (bRe :+ bIm) = dist
            where dGridRe = abs $ blRe - urRe
                  dGridIm = abs $ blIm - urIm
                  dRe     = abs $ bRe  - aRe
                  dIm     = abs $ bIm  - aIm
                  dW      = dRe / dGridRe * fromIntegral w
                  dH      = dIm / dGridIm * fromIntegral h
                  dist    = sqrt $ dW * dW + dH * dH                                                                

                                                                

fractalColorsOnGridKdt :: Fractal -> Int -> Int -> FractalPoint -> FractalPoint -> [(FractalPoint, FractalColor)] -> [(FractalPoint, FractalColor)]
fractalColorsOnGridKdt f w h bl ur oldColoredGrid = {- trace "---------" -} newColors
                                                    where distOnScreen = constructDistOnScreen w h bl ur 
                                                          newGrid = makeComplexGrid w h bl ur 
                                                          tree = KDM.build fractalPointAsList oldColoredGrid
                                                          newColors = if KDM.null tree then map (\p -> (p, fractalColorInPoint f p)) newGrid
                                                                      else 
                                                                      do p <- newGrid 
                                                                         let (p', color) = KDM.nearest tree p
                                                                         let dist = distOnScreen p p'
                                                                         if dist < nearestAcceptedDist then return {- $ trace "1" -}(p, color)
                                                                         else return {- $ trace "0" -} (p, fractalColorInPoint f p)