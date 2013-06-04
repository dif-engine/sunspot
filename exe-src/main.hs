{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import           Data.Array.Repa.Index 
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Filesystem.Path.CurrentOS (decodeString)
import           GHC.Float (double2Float)
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

type Picture a = Repa.Array Repa.U DIM2 a
type ColorPicture = Picture (RGBA Float)


computeQuadTree :: forall a. (VUM.Unbox a) => 
                  ([a] -> a) ->
                  Picture a -> IO [Picture a]
computeQuadTree reduction pict = do
  let (Z :. w0 :. h0) =  Repa.extent pict
  if | w0 <= 1 -> return [pict]
     | h0 <= 1 -> return [pict]
     | otherwise -> do
       let w = w0 `div` 2; h = h0 `div` 2
           at :: Int -> Int -> a
           at x y = Repa.unsafeIndex pict (ix2 x y)

           gen :: DIM2 -> a
           gen (Z :. x :. y) = reduction $
             at <$> [2*x, 2*x+1] <*> [2*y, 2*y+1]
       nextPict <- Repa.computeP $ Repa.fromFunction
         (ix2 w h) gen
       fmap (pict:) $ computeQuadTree reduction nextPict


data SunspotClass = NotSunspot | SunspotA | SunspotB | SunspotBG | SunspotBGD
  deriving (Eq, Ord, Show, Enum)

sscOf :: (Fractional a, Ord a) => Lens.Iso' (RGBA a) SunspotClass
sscOf = Lens.iso to fro 
  where
    to (RGBA (r,g,b,a))
       | r <  0.5 && g <  0.5 && b >= 0.5 = SunspotA
       | r <  0.5 && g >= 0.5 && b <  0.5 = SunspotB
       | r >= 0.5 && g >= 0.5 && b <  0.5 = SunspotBG
       | r >= 0.5 && g <  0.5 && b <  0.5 = SunspotBGD
       | otherwise                        = NotSunspot
    fro NotSunspot = rgba 1 1 1 1
    fro SunspotA   = rgba 0 0 1 1
    fro SunspotB   = rgba 0 1 0 1
    fro SunspotBG  = rgba 1 1 0 1
    fro SunspotBGD = rgba 1 0 0 1
    


loadColorPicture :: FilePath -> IO ColorPicture
loadColorPicture fn = withMagickWandGenesis $ do
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
     liftIO $ VUM.write pixels idx $ fmap double2Float $ rgba r0 g0 b0 a0

  pixelsF <- liftIO $ VU.freeze pixels

  return $ Repa.fromUnboxed (ix2 w h) pixelsF



main = do
  fns <- getArgs

  forM_ fns $ \fn -> do
    pictSun <- loadColorPicture fn
  
    
    classedSun <- Repa.computeP $ Repa.map (fromEnum . (^. sscOf)) pictSun
                  :: IO (Picture Int)

    forM_ [0..4] $ \classIdx -> do
      cnt <- Repa.foldAllP (+) (0::Int) $ Repa.map (\i -> if i==classIdx then 1 else 0) classedSun
      printf "class %d: %08d\n" classIdx cnt

    tree <- computeQuadTree (fmap (/4) . sum) pictSun
    
    print $ map Repa.extent tree

