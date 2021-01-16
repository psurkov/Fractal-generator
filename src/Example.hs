module Example
    ( someFunc
    ) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.ViewState
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe
import Data.Word
import Data.Complex
import qualified Data.ByteString as B
import Data.Semigroup

import Debug.Trace

width :: Int
width = 400

height :: Int
height = 400

window :: Display
window = InWindow "Nice Mandelbort Window" (fromIntegral width, fromIntegral height) (10, 10)

background :: Color
background = white

bitmapFormat :: BitmapFormat
bitmapFormat = BitmapFormat BottomToTop PxRGBA

newtype IterFunc = IterFunc {getFunc :: Complex Float -> Complex Float -> Complex Float}
type Checker = Complex Float -> Bool

instance Semigroup IterFunc where
    (IterFunc f) <> (IterFunc g) = IterFunc $ \z c -> f (g z c) c

check :: Checker
check c = magnitude c <= 2

mandelbrotIterFunc :: IterFunc 
mandelbrotIterFunc = IterFunc $ \z c -> z*z + c

makeComplexGrid :: Int -> Int -> Complex Float -> Complex Float -> [Complex Float]
makeComplexGrid w h bl ur = do
    imag <- make1DComplexGrid h (imagPart bl) (imagPart ur)
    real <- make1DComplexGrid w (realPart bl) (realPart ur)
    return (real :+ imag)
    where make1DComplexGrid n l r = (+l) . (/ fromIntegral n) . (*(r - l)) . fromIntegral <$> [0..n - 1]

blWorldByWidthHeight :: (Int, Int) -> Point
blWorldByWidthHeight (w, h) = (-fromIntegral w/2.0, -fromIntegral h/2.0)

urWorldByWidthHeight :: (Int, Int) -> Point
urWorldByWidthHeight (w, h) = (fromIntegral w/2.0, fromIntegral h/2.0)

blWorldInit :: Point
blWorldInit = blWorldByWidthHeight (width, height)

urWorldInit :: Point
urWorldInit = urWorldByWidthHeight (width, height)

blComplexInit :: Complex Float
blComplexInit = (-1) :+ (-1)

urComplexInit :: Complex Float
urComplexInit = (1) :+ (1)

convertWorldToComplex :: Point -> Complex Float
convertWorldToComplex (x, y) = xComplex :+ yComplex
                                where xRel = x / fst urWorldInit
                                      yRel = y / snd urWorldInit
                                      xComplex = xRel * realPart urComplexInit 
                                      yComplex = yRel * imagPart urComplexInit

makeColors :: Bool -> [Word8]
makeColors False = [100,0,0,255]
makeColors True  = [0,100,0,255]

fractal :: Integer -> Complex Float -> Complex Float -> Integer -> Integer -> B.ByteString
fractal n bl ur w h = B.pack $ (concat) 
                             $ (fmap) 
                               (makeColors . check . getFunc (stimes n mandelbrotIterFunc) 0)
                               (makeComplexGrid (fromIntegral w) (fromIntegral h) bl ur)

-- fractalArray :: [Complex Float]
-- fractalArray = makeComplexGrid width height ((-2) :+ (-1)) (1 :+ 1)

--makeFractalStep :: IterFunc -> Checker -> [Complex Float] -> [Complex Float]
--makeFractalStep f ch = fmap (\z c -> if ch c then f c else c) 

traces a = trace (show a) a

drawing :: Float -> (Complex Float, Complex Float) -> (Int, Int) -> Picture
drawing res (bl, ur) (width, height) = finalPic
                    where resW = floor $ fromIntegral width / res
                          resH = floor $ fromIntegral height / res
                          frac = fractal 40 bl ur resW resH
                          resPic = bitmapOfByteString (fromIntegral resW) (fromIntegral resH) bitmapFormat frac False
                          finalPic = scale res res resPic

data World = World { worldPic :: Picture, 
                     worldViewState :: ViewState,
                     c_blur :: (Complex Float, Complex Float),
                     worldScreenSize :: (Int, Int),
                     worldResolution :: Float,
                     lastResolutionUpdateTime :: Float
                   }

-- bl :: World -> Complex Float
-- bl = fst . blur
-- 
-- ur :: World -> Complex Float
-- ur = snd . blur

updatedViewStateInit = viewStateInitWithConfig config
                       where config = defaultCommandConfig

initResolution :: Float
initResolution = 4

initWorld :: World
initWorld = World (drawing initResolution (blComplexInit, urComplexInit) (fromIntegral width, fromIntegral height)) updatedViewStateInit (blComplexInit, urComplexInit) (fromIntegral width, fromIntegral height) initResolution 0



-- drawing2 bl ur = Pictures [drawing bl ur, circle 100, bitmapOfByteString 10 10 bitmapFormat (B.pack $ concatMap (const [0,0,100,255]) $ replicate 100 True) False]

updateWorld :: Event -> World -> World
updateWorld event@(EventResize worldScreenSize') (World pic worldViewState c_blur _ worldResolution lastResolutionUpdateTime) = updateWorldViewState event world'
        where world' = World pic worldViewState c_blur worldScreenSize' worldResolution lastResolutionUpdateTime
updateWorld event world = updateWorldViewState event world

updateWorldViewState :: Event -> World -> World
updateWorldViewState e world = World (drawing res' (bl, ur) (worldScreenSize world)) st' (bl, ur) (worldScreenSize world) res' (lastResolutionUpdateTime world)
                               where st' = fromMaybe (worldViewState world) (updateViewStateWithEventMaybe e (worldViewState world))
                                     vp = viewStateViewPort st'
                                     res' = if isJust (updateViewStateWithEventMaybe e (worldViewState world)) then initResolution else worldResolution world
                                     w_bl = blWorldByWidthHeight $ worldScreenSize world
                                     w_ur = urWorldByWidthHeight $ worldScreenSize world
                                     bl = traces $ convertWorldToComplex $ invertViewPort vp w_bl
                                     ur = traces $ convertWorldToComplex $ invertViewPort vp w_ur



timeToMaxResolution :: Float
timeToMaxResolution = 20

maxResolution :: Float
maxResolution = 1

updateTime :: Float -> World -> World
updateTime t (World pic st blur size res _) = World pic st blur size res' t
                                              where res' = traces $ max maxResolution $ res - t * (initResolution - maxResolution)

-- reverseUpdateViewPort :: ViewPort -> Picture -> Picture
-- reverseUpdateViewPort (ViewPort (dx, dy) sc rot) pic = -- pic
--                                                       translate (dx) (dy)
--                                                       -- $ scale (1/sc) (1/sc) 
--                                                       -- $ rotate (rot) 
--                                                       $ pic

someFunc :: IO ()
-- someFunc = display window background drawing
someFunc = play window background 24 initWorld worldPic updateWorld updateTime --updateWorld
-- (\vp t w -> trace (show $ convertWorldToComplex $ traces $ invertViewPort vp (100, 100)) w)
