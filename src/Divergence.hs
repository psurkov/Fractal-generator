{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Divergence where


import Data.Array.Accelerate as A
import Data.Array.Accelerate.Data.Complex as A
import qualified Prelude as P

import Geometry

-- f(z, c)
type IterFunc = Exp FractalPoint -> Exp FractalPoint -> Exp FractalPoint

type Checker = Exp FractalPoint -> Exp Bool

-- return n if diverges in n iterations, else -1
-- calcDivergenceInPoint :: Exp Int -> IterFunc -> Checker -> Exp FractalPoint -> Exp Int
-- calcDivergenceInPoint iters func checker point = snd $ while (\zi -> snd zi < iters &&
--                                                                checker (fst zi))
--                                                        (\zi -> step point zi)
--                                                        (lift (point, constant 0))
--     where
--         step c (unlift -> (z, i)) = lift (func z c, i + constant 1)

calcDivergenceInPoint :: Exp Int -> IterFunc -> Checker -> Exp FractalPoint -> Exp Int
calcDivergenceInPoint iters func checker point = helper $ snd $ while (\zi -> snd zi < iters &&
                                                               checker (fst zi))
                                                       (\zi -> step point zi)
                                                       (lift (point, constant 0))
    where
        step c (unlift -> (z, i)) = lift (func z c, i + constant 1)
        helper x = ifThenElse (iters == x) (-1) x