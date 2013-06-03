module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Filesystem.Path.CurrentOS
import Graphics.ImageMagick.MagickWand
import System.Environment
import Text.Printf



main = do
  [fn] <- getArgs
  withMagickWandGenesis $ do
    (_,img) <- magickWand
    readImage img $ decodeString fn
    w <- getImageWidth img
    h <- getImageWidth img
    liftIO $ printf "(%d, %d)\n" w h

    pxl <- pixelWand

    forM_ [0..w-1] $ \x-> do
      let y :: Int
          y = 1024

      getImagePixelColor img x y pxl
      r <- getRed pxl
      liftIO $ print r

    return ()
