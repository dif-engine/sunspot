#!/usr/bin/env runhaskell
import Control.Monad
import System.Random

import Text.Printf

main :: IO ()
main = replicateM_ 1000 $ do
  x <- randomRIO (-4, 4)
  y <- randomRIO (-2, 2)
  let z :: Double
      z = y - sin(x)
  printf "%f 1:%f 2:%f\n" z x y
