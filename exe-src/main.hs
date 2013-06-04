{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import           Control.Applicative
import           Control.DeepSeq (deepseq, NFData(..), force)
import           Control.Lens ((^.), Lens')
import qualified Control.Lens as Lens
import           Control.Monad
import           Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Eval as Repa
import           Data.Array.Repa.Index (ix2, DIM2)
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Filesystem.Path.CurrentOS (decodeString)
import           Graphics.ImageMagick.MagickWand
import           System.Environment
import           System.IO
import           Text.Printf



newtype RGBA a = RGBA {unRGBA :: (a,a,a,a)} deriving (Eq, Show, Functor)
deriving instance (VG.Vector VU.Vector a, VUM.Unbox a) => VG.Vector VU.Vector (RGBA a)
deriving instance (VGM.MVector VU.MVector a, VUM.Unbox a) => VGM.MVector VU.MVector (RGBA a)
deriving instance (VUM.Unbox a) => VUM.Unbox (RGBA a)
deriving instance (Repa.Elt a) => Repa.Elt (RGBA a)

instance (NFData a) => NFData (RGBA a) where
  rnf (RGBA a4) = rnf a4 `seq` ()

{-# INLINE isoRGBA #-}
isoRGBA :: Lens.Iso' (RGBA a) (a,a,a,a)
isoRGBA = Lens.iso unRGBA RGBA


red, green, blue, alpha :: Lens' (RGBA a) a
{-# INLINE red #-} 
red = isoRGBA . Lens._1
{-# INLINE green #-} 
green = isoRGBA . Lens._2
{-# INLINE blue #-} 
blue = isoRGBA . Lens._3
{-# INLINE alpha #-}
alpha = isoRGBA . Lens._4

{-# INLINE rgba #-}
rgba r g b a = RGBA (r,g,b,a)

{-# INLINE zipRGBAWith #-}
zipRGBAWith :: (NFData a) => (a->a->a) -> RGBA a -> RGBA a -> RGBA a 
zipRGBAWith f (RGBA (r0,b0,g0,a0)) (RGBA (r1,b1,g1,a1)) = 
  force $ rgba (r0`f`r1) (g0`f`g1) (b0`f`b1) (a0`f`a1)

instance (NFData a, Num a) => Num (RGBA a) where
  {-# INLINE fromInteger #-}
  fromInteger n' = force $ let n = fromInteger n' in rgba n n n n
  {-# INLINE (+) #-}
  (+) = zipRGBAWith (+)
  {-# INLINE (-) #-}
  (-) = zipRGBAWith (-)
  (*) = error "cannot multiply"
  {-# INLINE negate #-}
  negate = force $ fmap negate
  abs = error "cannot abs"
  signum = error "cannot sig"

type Image = Repa.Array Repa.U DIM2 (RGBA Double)

loadImage :: FilePath -> IO Image
loadImage fn = withMagickWandGenesis $ do
  (_,img) <- magickWand
  readImage img $ decodeString fn
  w <- getImageWidth img
  h <- getImageWidth img
  liftIO $ printf "%s (%d, %d)\n" fn w h

  pxl <- pixelWand
  pixels <- liftIO $ VUM.new (h*w) 

  forM_ [0..(h*w-1)] $ \idx -> do
     let (y,x) = idx `divMod` w
     getImagePixelColor img x y pxl
     r0 <- getRed pxl
     g0 <- getGreen pxl
     b0 <- getBlue pxl
     a0 <- getAlpha pxl
     liftIO $ VUM.write pixels idx $ rgba r0 g0 b0 a0

  pixelsF <- liftIO $ VU.freeze pixels

  return $ Repa.fromUnboxed (ix2 w h) pixelsF



main = do
  [fn] <- getArgs

  repaSun <- loadImage fn

  nume <- Repa.foldAllP (+) 0 repaSun
  deno <- Repa.foldAllP (+) 0 $ Repa.map (const (1::Double)) repaSun
  printf "%s %s" (show nume) (show deno)
  return ()
