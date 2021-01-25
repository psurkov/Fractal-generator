{-# LANGUAGE StandaloneDeriving #-}
module View where

import Data.Maybe
import Control.Monad
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.ViewState
import Graphics.Gloss.Interface.Pure.Game

import Debug.Trace

import Geometry
import FractalColour
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
                          pic = packColoursToPicture $ fractalColoursOnGrid fractal resW resH bl ur 
                          -- resPic = bitmapOfByteString (fromIntegral resW) (fromIntegral resH) bitmapFormat frac False
                          finalPic = scale res res pic

data ViewSettings = ViewSettings { worldScreenSize :: (Int, Int),
                                   mousePosition :: (Float, Float),
                                   keysPressed :: [UsefulKey],
                                   resolution :: Float,
                                   zoomSpeed :: Float
                                 }
                                   

data World = World { worldPic :: Picture, 
                     c_blur :: (FractalPoint, FractalPoint),
                     viewSettings :: ViewSettings
                   }

-- bl :: World -> FractalPoint
-- bl = fst . blur
-- 
-- ur :: World -> FractalPoint
-- ur = snd . blur

updatedViewStateInit = viewStateInitWithConfig config
                       where config = defaultCommandConfig

initViewSettings = ViewSettings { worldScreenSize = (fromIntegral width, fromIntegral height),
                                  mousePosition   = (0, 0),
                                  keysPressed     = [],
                                  resolution      = 1,
                                  zoomSpeed       = 0.75
                                }


initWorld :: World
initWorld = World { worldPic     = drawing mandelbrotFractal (resolution initViewSettings) (blComplexInit, urComplexInit) (fromIntegral width, fromIntegral height),
                    c_blur       = (blComplexInit, urComplexInit),
                    viewSettings = initViewSettings
                  }



-- drawing2 bl ur = Pictures [drawing bl ur, circle 100, bitmapOfByteString 10 10 bitmapFormat (B.pack $ concatMap (const [0,0,100,255]) $ replicate 100 True) False]

eventHandler :: Event -> World -> World
eventHandler event@(EventResize worldScreenSize') world = updateWorldViewSettings event world'
        where viewSettings' = (viewSettings world) { worldScreenSize = worldScreenSize' }
              world'        = world { viewSettings = viewSettings' }
eventHandler event world = updateWorldViewSettings event world        

-- debug:
deriving instance Show ViewPort
deriving instance Eq ViewPort
deriving instance Eq ViewState
deriving instance Show ViewState
deriving instance Eq ViewSettings
deriving instance Eq World

newtype UsefulKey = UsefulKey (Key, Maybe Modifiers, Maybe (Float, Float))
instance Eq UsefulKey where
    UsefulKey (key1, mod1, pos1) == UsefulKey (key2, mod2, pos2) = key1 == key2 
--                                                                && (isNothing mod1 || isNothing mod2 || mod1 == mod2) 
--                                                                && (isNothing pos1 || isNothing pos2 || pos1 == pos2)

zoom :: Float -> World -> World
zoom dt world = world'
              where pos = mousePosition $ viewSettings world
                    screenSize = worldScreenSize $ viewSettings world
                    speed = zoomSpeed $ viewSettings world
--                    w_bl = blWorldByWidthHeight screenSize
                    w_ur = urWorldByWidthHeight screenSize
                    (bl, ur) = c_blur world
                    c_click = traces $ convertWorldToComplex w_ur ur $ mousePosition $ viewSettings world
                    c_size = (ur - bl) / realToFrac 2
                    ur_ideal = c_click + c_size * realToFrac (1 / (1 + speed))
                    bl_ideal = c_click - c_size * realToFrac (1 / (1 + speed))
                    d_ur = ur_ideal - ur
                    d_bl = bl_ideal - bl
                    bl' = bl + d_bl * realToFrac (dt * speed)
                    ur' = ur + d_ur * realToFrac (dt * speed)
                    c_blur' = (bl', ur')
                    world' = world { c_blur = c_blur' }
--                    bl = traces $ convertWorldToComplex $ invertViewPort vp w_bl'
--                    ur = traces $ convertWorldToComplex $ invertViewPort vp w_ur'

unzoom :: Float -> World -> World
unzoom dt = id


-- change World, but not worldPic
ukeyHandler :: UsefulKey -> Float -> World -> World
ukeyHandler ukey | ukey == UsefulKey (MouseButton LeftButton, Nothing, Nothing) = zoom
                 | ukey == UsefulKey (MouseButton LeftButton, Nothing, Nothing) = unzoom
ukeyHandler _ = const id

updateKeysPressed :: Event -> [UsefulKey] -> [UsefulKey]
updateKeysPressed event@(EventKey key keyState modifiers pos) ukeys | keyState == Down && ukey `notElem` ukeys = ukey:ukeys 
                                                                    | keyState == Up   = filter (/= ukey) ukeys
                where ukey = UsefulKey (key, Just modifiers, Just pos)
updateKeysPressed _                                 events = events

updateMousePosition :: Point -> Event -> Point
updateMousePosition pos (EventMotion pos') = pos'
updateMousePosition pos (EventKey _ _ _ pos') = pos'
updateMousePosition pos _                = pos                  

updateWorldViewSettings :: Event -> World -> World
updateWorldViewSettings e world = world'
                               where keysPressed' = updateKeysPressed (traces e) $ keysPressed $ viewSettings world
                                     mousePosition' = updateMousePosition (mousePosition $ viewSettings world) e
                                     viewSettings' = (viewSettings world) { keysPressed = keysPressed', mousePosition = mousePosition' }
--                                     worldPic' = drawing mandelbrotFractal (resolution $ viewSettings world) (c_blur world) (worldScreenSize $ viewSettings world)
                                     world' = world {{-worldPic = worldPic', -}viewSettings = viewSettings'}
                                 -- st' = fromMaybe (worldViewState world) (updateViewStateWithEventMaybe (traces e) (worldViewState world))
                                     
                                       -- vp = viewStateViewPort st'
                                     
                                       -- w_bl' = blWorldByWidthHeight $ worldScreenSize world
                                     
                                       -- w_ur' = urWorldByWidthHeight $ worldScreenSize world
                                     
                                       -- bl = traces $ convertWorldToComplex $ invertViewPort vp w_bl'
                                     
                                       -- ur = traces $ convertWorldToComplex $ invertViewPort vp w_ur'

                                     
timeHandler :: Float -> World -> World
timeHandler dt world = worldRedrawn
                 where ukeys = keysPressed $ viewSettings world
                       ukeysFuncs = map (`ukeyHandler` dt) ukeys
                       ukeysFuncComposition = foldl (.) id ukeysFuncs
                       worldFrozen  = ukeysFuncComposition world
                       worldPic' = drawing mandelbrotFractal (resolution $ viewSettings world) (c_blur world) (worldScreenSize $ viewSettings world)
                       worldRedrawn = worldFrozen { worldPic = worldPic' }
                       




-- reverseUpdateViewPort :: ViewPort -> Picture -> Picture
-- reverseUpdateViewPort (ViewPort (dx, dy) sc rot) pic = -- pic
--                                                       translate (dx) (dy)
--                                                       -- $ scale (1/sc) (1/sc) 
--                                                       -- $ rotate (rot) 
--                                                       $ pic

viewMain :: IO ()
viewMain = play window background 60 initWorld worldPic eventHandler timeHandler