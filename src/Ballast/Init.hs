module Ballast.Init(run) where

import           Ballast.AssetLoader
import           Ballast.GameLoop
import           Ballast.Internal
import           Ballast.Types
import           SDL


run :: (Show c, Cargo c) => BallastConfig -> c -> IO ()
run bc cargo = do
  initializeAll
  window <- createWindow (bcTitle bc) $
            defaultWindow { windowInitialSize = fmap fromIntegral $ bcPixelDimensions bc }
  renderer <- createRenderer window (-1) defaultRenderer

  ballastState <-loadAssets renderer bc
  initialTick <- ticks
  let initialLag = 0
  gameLoop renderer ballastState cargo initialTick initialLag
  -- do we need to deinitialize?
