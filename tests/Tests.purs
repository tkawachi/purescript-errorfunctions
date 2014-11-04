module Main where

import Global(infinity)
import Data.Array
import Data.Maybe
import Math(abs)

import Debug.Trace

-- Import the library's module(s)
import Numeric.ErrorFunction

-- Import Test.QuickCheck, which supports property-based testing
import Test.QuickCheck

epsilon :: Number
epsilon = 1e-5

(=~) :: Number -> Number -> Boolean
(=~) x y = abs (x - y) < epsilon

main = do
  trace "-1 <= erf n <= 1"
  quickCheck $ \n -> -1 <= erf n && erf n <= 1

  trace "erf (-n) = - (erf n)"
  quickCheck $ \n -> erf (-n) =~ (- erf n)

  trace "0 <= erfc n <= 2"
  quickCheck $ \n -> 0 <= erfc n && erfc n <= 2

  trace "erf 0 == 0"
  quickCheck' 1 $ (erf 0 =~ 0)

  trace "erf infinity == 1"
  quickCheck' 1 $ (erf infinity =~ 1)

  trace "erf -infinity == -1"
  quickCheck' 1 $ (erf (-infinity) =~ -1)

  -- taken from http://ja.wikipedia.org/wiki/%E8%AA%A4%E5%B7%AE%E9%96%A2%E6%95%B0
  quickCheck' 1 $ (erf 0.05 =~ 0.0563720)
  quickCheck' 1 $ (erf 0.10 =~ 0.1124629)
  quickCheck' 1 $ (erf 0.15 =~ 0.1679960)
  quickCheck' 1 $ (erf 0.20 =~ 0.2227026)
  quickCheck' 1 $ (erf 0.25 =~ 0.2763264)
  quickCheck' 1 $ (erf 0.30 =~ 0.3286268)
  quickCheck' 1 $ (erf 0.90 =~ 0.7969082)
  quickCheck' 1 $ (erf 1.00 =~ 0.8427008)
  quickCheck' 1 $ (erf 2.00 =~ 0.9953223)
  quickCheck' 1 $ (erf 3.00 =~ 0.9999779)
  
