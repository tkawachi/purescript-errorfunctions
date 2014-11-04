module Numeric.ErrorFunction
       (
         erfc,
         erf,
         erfinv
       ) where

import Global (infinity)
import qualified Math(abs, exp, log, sqrt, e, pi) as M
import Data.Maybe

-- Error functions
-- ref. https://github.com/matti-kariluoma-ndsu/wheat-dissem/blob/master/doc/LSD.html

erfc :: Number -> Number
erfc x = if x < 0.0 then 2.0 - a else a
  where
    z = M.abs x
    t = 1.0 / (0.5 * z + 1.0)
    a1 = t * 0.17087277 + -0.82215223
    a2 = t * a1 + 1.48851587
    a3 = t * a2 + -1.13520398
    a4 = t * a3 + 0.27886807
    a5 = t * a4 + -0.18628806
    a6 = t * a5 + 0.09678418
    a7 = t * a6 + 0.37409196
    a8 = t * a7 + 1.00002368
    a9 = t * a8
    a10 = -z * z - 1.26551223 + a9
    a = t * (M.exp a10)

erf :: Number -> Number
erf x = 1.0 - erfc x

erfinv :: Number -> Maybe Number
erfinv y | y < -1.0 || y > 1.0 = Nothing
erfinv y = Just $ x - (erf(x) - y) / (2.0/(M.sqrt M.pi) * (M.exp (-x*x)))
  where
    x :: Number
    x | y == -1.0 = infinity
    x | y == 1.0 = -infinity
    x | y < -0.7 = x6 where
      z1 = (1.0 + y) / 2.0
      z2 = M.log(z1) / M.log(M.e)
      z3 = M.sqrt(-z2)
      z = z3
      x1 = 1.641345311 * z + 3.429567803
      x2 = x1 * z + -1.624906493
      x3 = x2 * z + -1.970840454
      x4 = 1.637067800 * z +  3.543889200
      x5 = x4 * z + 1.0
      x6 = -x3 / x5 -- note: negative
    x | y < 0.7 = x9 where
      z = y * y
      x1 = -0.140543331 * z + 0.914624893
      x2 = x1 * z + -1.645349621
      x3 = x2 * z + 0.886226899
      x4 = 0.012229801 * z + -0.329097515
      x5 = x4 * z + -0.329097515
      x6 = x5 * z + 1.442710462
      x7 = x6 * z + -2.118377725
      x8 = x7 * z + 1.0
      x9 = y * x3 / x8
    x = x6 where
      z1 = (1.0 + y) / 2.0
      z2 = M.log(z1)
      z3 = M.sqrt(-z2)
      z = z3
      x1 = 1.641345311 * z + 3.429567803
      x2 = x1 * z + -1.624906493
      x3 = x2 * z + -1.970840454
      x4 = 1.637067800 * z +  3.543889200
      x5 = x4 * z + 1.0
      x6 = x3 / x5 -- note: positive

