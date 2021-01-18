module GridScale where
{- 

import Data.Complex
import Data.KdMap.Static

import Geometry
import Fractal (Fractal, fractalColorInPoint)
import FractalColor


nearestAcceptedDist :: Float
nearestAcceptedDist = 0.5

constructDistOnScreen :: Int -> Int -> FractalPoint -> FractalPoint -> (FractalPoint -> FractalPoint -> Float)
constructDistOnScreen w h (blRe :+ blIm) (urRe :+ urIm) (aRe :+ aIm) (bRe :+ bIm) = dist
            where dGridRe = abs $ blRe - urRe
                  dGridIm = abs $ blIm - urIm
                  dRe     = abs $ bRe  - aRe
                  dIm     = abs $ bIm  - aIm
                  dW      = dGridRe / dRe * fromIntegral w
                  dH      = dGridIm / dIm * fromIntegral h
                  dist    = sqrt $ dW * dW + dH * dH                                                                

                                                                

fractalColorsOnGridKdt :: [(FractalPoint, FractalColor)] -> Fractal -> Int -> Int -> FractalPoint -> FractalPoint -> [FractalColor]
fractalColorsOnGridKdt oldColoredGrid f w h bl ur = newColors
                                                    where distOnScreen = constructDistOnScreen w h bl ur 
                                                          newGrid = makeComplexGrid w h bl ur 
                                                          tree = build fractalPointAsList oldColoredGrid
                                                          newColors = do p <- newGrid 
                                                                         let (p', color) = nearest tree p
                                                                         if distOnScreen p p' < nearestAcceptedDist then return color
                                                                         else return $ fractalColorInPoint f p
-}
