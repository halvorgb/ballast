module Ballast.Init(run) where

import           Ballast.GameLoop
import           Ballast.Types
import           SDL

run :: (Show c, Cargo c) => BallastConfig -> c -> IO ()
run bc cargo = do
  initializeAll
  window <- createWindow (bcTitle bc) defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer

  initialTick <- ticks
  let initialLag = 0
  gameLoop renderer bc cargo initialTick initialLag
  -- do we need to deinitialize?
