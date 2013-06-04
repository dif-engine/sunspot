module Main where

import Control.Applicative
import Control.DeepSeq (NFData)
import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.IORef.Strict as SIO
import Data.IORef
import Filesystem.Path.CurrentOS
import Graphics.ImageMagick.MagickWand
import qualified System.IO.Strict as SIO
import qualified System.IO.Strict.Internals as SIO
import System.Environment
import Text.Printf

liftSIO :: (MonadIO m, NFData a) => SIO.SIO a -> m a
liftSIO = liftIO . SIO.run

main = do
  [fn] <- getArgs
  withMagickWandGenesis $ do
    (_,img) <- magickWand
    readImage img $ decodeString fn
    w <- getImageWidth img
    h <- getImageWidth img
    liftIO $ printf "(%d, %d)\n" w h

    sioCtrRef <- liftIO $ newIORef undefined

    pxl <- pixelWand
    liftSIO $ do
      sioCtr <- SIO.newIORef (0 :: Double)
      SIO.wrap0 $ writeIORef sioCtrRef sioCtr
      return ()

    sioCtr <- liftIO $ readIORef sioCtrRef

    forM_ [0..h-1] $ \y-> do
      forM_ [0..w-1] $ \x-> do
        getImagePixelColor img x y pxl
        r <- getRed pxl
        liftSIO $ SIO.modifyIORef sioCtr (r+)

    liftSIO $ do
      sum <- SIO.readIORef sioCtr
      SIO.wrap0 $ print sum

    return ()
