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

width :: Integer
width = 400

height :: Integer
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
makeComplexGrid w h lb ur = do
    imag <- make1DComplexGrid h (imagPart lb) (imagPart ur)
    real <- make1DComplexGrid w (realPart lb) (realPart ur)
    return (real :+ imag)
    where make1DComplexGrid n l r = (+l) <$> (/ fromIntegral n) <$> (*(r - l)) <$> fromIntegral <$>[0..n - 1]


blWorldInit :: Point
blWorldInit = (-fromIntegral width/2.0, -fromIntegral height/2.0)

urWorldInit :: Point
urWorldInit = (fromIntegral width/2.0, fromIntegral height/2.0)

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

drawing :: Float -> Complex Float -> Complex Float -> Picture
drawing res bl ur = finalPic
                    where resW = floor $ fromIntegral width / res
                          resH = floor $ fromIntegral height / res
                          frac = fractal 40 bl ur resW resH
                          resPic = bitmapOfByteString (fromIntegral resW) (fromIntegral resH) bitmapFormat frac False
                          finalPic = scale res res resPic

data World = World { pic :: Picture, bl :: Complex Float, ur :: Complex Float, worldViewState :: ViewState }


updatedViewStateInit = viewStateInitWithConfig config
                       where config = defaultCommandConfig

resolution :: Float
resolution = 2

initWorld :: World
initWorld = World (drawing resolution blComplexInit urComplexInit) blComplexInit urComplexInit updatedViewStateInit



-- drawing2 bl ur = Pictures [drawing bl ur, circle 100, bitmapOfByteString 10 10 bitmapFormat (B.pack $ concatMap (const [0,0,100,255]) $ replicate 100 True) False]

updateWorld :: Event -> World -> World
updateWorld e w = World (drawing resolution bl ur) bl ur st'
                  where st' = fromMaybe (worldViewState w) (updateViewStateWithEventMaybe e (worldViewState w))
                        vp = viewStateViewPort st'
                        bl = traces $ convertWorldToComplex $ invertViewPort vp (-fromIntegral width/2, -fromIntegral height/2)
                        ur = traces $ convertWorldToComplex $ invertViewPort vp (fromIntegral width/2, fromIntegral height/2)



-- reverseUpdateViewPort :: ViewPort -> Picture -> Picture
-- reverseUpdateViewPort (ViewPort (dx, dy) sc rot) pic = -- pic
--                                                       translate (dx) (dy)
--                                                       -- $ scale (1/sc) (1/sc) 
--                                                       -- $ rotate (rot) 
--                                                       $ pic

someFunc :: IO ()
-- someFunc = display window background drawing
someFunc = play window background 60 initWorld pic updateWorld (\t w -> w) --updateWorld
-- (\vp t w -> trace (show $ convertWorldToComplex $ traces $ invertViewPort vp (100, 100)) w)
