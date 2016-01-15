module Ballast.Init(run) where

import           Ballast.GameLoop
import           Ballast.Types
import           SDL
import Ballast.AssetLoader

run :: (Show c, Cargo c) => BallastConfig -> c -> IO ()
run bc cargo = do
  initializeAll
  window <- createWindow (bcTitle bc) defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer

  loadedBc <-loadAssets renderer bc
  initialTick <- ticks
  let initialLag = 0
  gameLoop renderer loadedBc cargo initialTick initialLag
  -- do we need to deinitialize?
