import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.String.Utils (replace)
import Filesystem.Path.CurrentOS
import Graphics.ImageMagick.MagickWand
import System.Environment
import System.IO
import System.Process
import Text.Printf

readInteractiveCommand :: String -> IO String
readInteractiveCommand cmd = do
  (_, stdout, _, _) <- runInteractiveCommand cmd
  hGetContents stdout

main = do
  _ <- system "mkdir -p data-shrunk"
  fns <- fmap words $ readInteractiveCommand $ "ls data/*.png"
  forM_ fns $ \fn -> withMagickWandGenesis $ do
    let fnOut = replace "data" "data-shrunk" fn

    liftIO $ do 
      printf "%s -> %s\n" fn fnOut
      hFlush stdout

    (_,w) <- magickWand
    readImage w $ decodeString fn

    magickIterate w $ \p -> do
        resizeImage p 512 512 lanczosFilter 1.0
        return ()
    writeImages w (decodeString fnOut) True

    return ()

