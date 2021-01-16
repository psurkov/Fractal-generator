module Example
    ( someFunc
    ) where

import Graphics.Gloss
import Data.Word
import Data.Complex
import qualified Data.ByteString as B

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

type Iter = Complex Float -> Complex Float -> Complex Float
type Checker = Complex Float -> Bool

iter :: Iter
iter z c = z*z + c

absC :: Complex Float -> Float
absC (re :+ im) = sqrt $ re*re + im*im

check :: Checker
check c = absC c <= 2

repeatIterN :: Iter -> Checker -> Integer -> Complex Float -> Complex Float -> Bool
repeatIterN _ _  n _ _ | n < 0 = error "Negative number of iterations"
repeatIterN _ ch 0 z _ = ch z
repeatIterN f ch n z c = repeatIterN f ch (n - 1) (f z c) c

-- direction? bruh, traverse go brrr
makeComplexGrid :: Int -> Int -> Complex Float -> Complex Float -> [[Complex Float]]
makeComplexGrid _ 0 _  _  = []
makeComplexGrid 0 _ _  _  = []
makeComplexGrid w h lb ur = makeComplexGrid' w h lb ur ((realPart ur - realPart lb)/fromIntegral w :+ 0) (0 :+ (imagPart ur - imagPart lb)/fromIntegral h)

makeComplexGrid' _ 0 _ _ _ _ = []
makeComplexGrid' w h lb ur wstep hstep = make1DComplexGrid lb w wstep : makeComplexGrid' w (h-1) (lb + hstep) ur wstep hstep
make1DComplexGrid _ 0   _    = []
make1DComplexGrid c len step = c : make1DComplexGrid (c + step) (len - 1) step

makeColors :: Bool -> [Word8]
makeColors False = [100,0,0,255]
makeColors True  = [0,100,0,255]

fractal :: Integer -> B.ByteString
fractal n = B.pack $ (concat . concat) 
                   $ (fmap . fmap) 
                     (makeColors . repeatIterN iter check n 0)
                     (makeComplexGrid width height ((-2) :+ (-1)) (1 :+ 1))

drawing :: Picture
-- drawing = bitmapOfByteString width height bitmapFormat (B.pack $ concat $ replicate (width * height) [0,100,0,255]) False
drawing = bitmapOfByteString width height bitmapFormat (fractal 40) False

someFunc :: IO ()
someFunc = display window background drawing