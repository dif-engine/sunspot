module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Data.IORef
import qualified Data.Array.Repa as Repa
import           Data.Array.Repa.Index (ix2)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Filesystem.Path.CurrentOS (decodeString)
import           Graphics.ImageMagick.MagickWand
import           System.Environment
import           System.IO
import           Text.Printf


main = do
  [fn] <- getArgs
  withMagickWandGenesis $ do
    (_,img) <- magickWand
    readImage img $ decodeString fn
    w <- getImageWidth img
    h <- getImageWidth img
    liftIO $ printf "(%d, %d)\n" w h



    pxl <- pixelWand
    pixels <- liftIO $ VUM.replicate (h*w) (0::Double)

    forM_ [0..(h*w-1)] $ \idx -> do
       let (y,x) = idx `divMod` w
       getImagePixelColor img x y pxl
       r <- getRed pxl
       g <- getGreen pxl
       b <- getBlue pxl
       liftIO $ VUM.write pixels idx $ (r+g+b)/3
 
    pixelsF <- liftIO $ VU.freeze pixels

    let repaSun = Repa.fromUnboxed (ix2 w h) pixelsF

    liftIO $ do
      nume <- Repa.foldAllP (+) 0 repaSun
      deno <- Repa.foldAllP (+) 0 $ Repa.map (const (1::Double)) repaSun
      printf "%f / %f = %f" nume deno (nume/deno)
    return ()
