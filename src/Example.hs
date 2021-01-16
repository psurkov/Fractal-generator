module Example
    ( someFunc
    ) where

import Graphics.Gloss
import Data.Word
import Data.Complex
import qualified Data.ByteString as B
import Data.Semigroup

width :: Int
width = 800

height :: Int
height = 800

window :: Display
window = InWindow "Nice Mandelbort Window" (width, height) (10, 10)

background :: Color
background = white

bitmapFormat :: BitmapFormat
bitmapFormat = BitmapFormat TopToBottom PxRGBA

newtype IterFunc = IterFunc {getFunc :: Complex Float -> Complex Float -> Complex Float}
type Checker = Complex Float -> Bool

instance Semigroup IterFunc where
    (IterFunc f) <> (IterFunc g) = IterFunc $ \z c -> f (g z c) c

check :: Checker
check c = magnitude c <= 2

mandelbrotIterFunc :: IterFunc 
mandelbrotIterFunc = IterFunc $ \z c -> z*z + c

-- direction? bruh, traverse go brrr
makeComplexGrid :: Int -> Int -> Complex Float -> Complex Float -> [Complex Float]
makeComplexGrid _ 0 _  _  = []
makeComplexGrid 0 _ _  _  = []
makeComplexGrid w h lb ur = concat $ makeComplexGrid' w h lb ur ((realPart ur - realPart lb)/fromIntegral w :+ 0) (0 :+ (imagPart ur - imagPart lb)/fromIntegral h)

makeComplexGrid' _ 0 _ _ _ _ = []
makeComplexGrid' w h lb ur wstep hstep = make1DComplexGrid lb w wstep : makeComplexGrid' w (h-1) (lb + hstep) ur wstep hstep
make1DComplexGrid _ 0   _    = []
make1DComplexGrid c len step = c : make1DComplexGrid (c + step) (len - 1) step
-- makeComplexGrid :: Int -> Int -> Complex Float -> Complex Float -> [Complex Float]
-- makeComplexGrid w h lb ur = do
--     real <- make1DComplexGrid w (realPart lb) (realPart ur)
--     imag <- make1DComplexGrid h (imagPart lb) (imagPart ur)
--     return (real :+ imag)
-- make1DComplexGrid 0 _ _ = []
-- make1DComplexGrid n l r = [l, (r - l) / fromIntegral n..r]

makeColors :: Bool -> [Word8]
makeColors False = [100,0,0,255]
makeColors True  = [0,100,0,255]

fractal :: Integer -> B.ByteString
fractal n = B.pack $ (concat) 
                   $ (fmap) 
                     (makeColors . check . getFunc (stimes n mandelbrotIterFunc) 0)
                     (makeComplexGrid width height ((-2) :+ (-1)) (1 :+ 1))

drawing :: Picture
-- drawing = bitmapOfByteString width height bitmapFormat (B.pack $ concat $ replicate (width * height) [0,100,0,255]) False
drawing = bitmapOfByteString width height bitmapFormat (fractal 40) False

someFunc :: IO ()
someFunc = display window background drawing