module View where

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.ViewState
import Graphics.Gloss.Interface.Pure.Game

import Debug.Trace

import Geometry
import FractalColor
import Fractal

import Mandelbrot


traces a = trace (show a) a


window :: Display
window = InWindow "Nice Mandelbort Window" (fromIntegral width, fromIntegral height) (10, 10)

background :: Color
background = white

bitmapFormat :: BitmapFormat
bitmapFormat = BitmapFormat BottomToTop PxRGBA

drawing :: Fractal -> Float -> (FractalPoint, FractalPoint) -> (Int, Int) -> Picture
drawing fractal res (bl, ur) (width, height) = finalPic
                    where resW = floor $ fromIntegral width / res
                          resH = floor $ fromIntegral height / res
                          frac = packColorsToByteString $ fractalColorsOnGrid fractal resW resH bl ur 
                          resPic = bitmapOfByteString (fromIntegral resW) (fromIntegral resH) bitmapFormat frac False
                          finalPic = scale res res resPic

data World = World { worldPic :: Picture, 
                     worldViewState :: ViewState,
                     c_blur :: (FractalPoint, FractalPoint),
                     worldScreenSize :: (Int, Int)
                   }

-- bl :: World -> FractalPoint
-- bl = fst . blur
-- 
-- ur :: World -> FractalPoint
-- ur = snd . blur

updatedViewStateInit = viewStateInitWithConfig config
                       where config = defaultCommandConfig

resolution :: Float
resolution = 1

initWorld :: World
initWorld = World (drawing mandelbrotFractal resolution (blComplexInit, urComplexInit) (fromIntegral width, fromIntegral height)) updatedViewStateInit (blComplexInit, urComplexInit) (fromIntegral width, fromIntegral height)



-- drawing2 bl ur = Pictures [drawing bl ur, circle 100, bitmapOfByteString 10 10 bitmapFormat (B.pack $ concatMap (const [0,0,100,255]) $ replicate 100 True) False]

updateWorld :: Event -> World -> World
updateWorld event@(EventResize worldScreenSize') (World pic worldViewState c_blur _) = updateWorldViewState event world'
        where world' = World pic worldViewState c_blur worldScreenSize'
updateWorld event world = updateWorldViewState event world        

updateWorldViewState :: Event -> World -> World
updateWorldViewState e world = World (drawing mandelbrotFractal resolution (bl, ur) (worldScreenSize world)) st' (bl, ur) (worldScreenSize world)
                               where st' = fromMaybe (worldViewState world) (updateViewStateWithEventMaybe e (worldViewState world))
                                     vp = viewStateViewPort st'
                                     w_bl = blWorldByWidthHeight $ worldScreenSize world
                                     w_ur = urWorldByWidthHeight $ worldScreenSize world
                                     bl = traces $ convertWorldToComplex $ invertViewPort vp w_bl
                                     ur = traces $ convertWorldToComplex $ invertViewPort vp w_ur





-- reverseUpdateViewPort :: ViewPort -> Picture -> Picture
-- reverseUpdateViewPort (ViewPort (dx, dy) sc rot) pic = -- pic
--                                                       translate (dx) (dy)
--                                                       -- $ scale (1/sc) (1/sc) 
--                                                       -- $ rotate (rot) 
--                                                       $ pic

viewMain :: IO ()
viewMain = play window background 60 initWorld worldPic updateWorld (\t w -> w) --updateWorld
-- (\vp t w -> trace (show $ convertWorldToComplex $ traces $ invertViewPort vp (100, 100)) w)