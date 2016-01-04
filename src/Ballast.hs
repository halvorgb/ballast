module Ballast (module Ballast.Types, run) where

import           Ballast.Render
import           Ballast.Types
import           Control.Monad  (unless)
import qualified Data.Text      as T
import qualified Data.Word      as W
import           Linear
import           Linear.Affine
import           SDL
import           SDL.Time

-- 1000/60 = 16.666666 --> slightly faster than 60 fps
millisecondsPerUpdate :: W.Word32
millisecondsPerUpdate = 16

run :: Cargo c => BallastConfig -> c -> IO ()
run bc cargo = do
  initializeAll
  window <- createWindow (bcTitle bc) defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer

  initialTick <- ticks
  let initialLag = 0
  gameLoop renderer bc cargo initialTick initialLag
  -- do we need to deinitialize?

-- | Gameloop: http://gameprogrammingpatterns.com/game-loop.html
--  ticks = milliseconds since initialization, elapedTicks is the delay (delta) inbetween each iteration.
gameLoop :: Cargo c => Renderer -> BallastConfig -> c -> W.Word32 -> W.Word32 -> IO ()
gameLoop renderer bc previousCargo previousTick previousLag = do
  -- Poll ticks.
  currentTick <- ticks
  let elapsedTicks = currentTick - previousTick

  -- poll and handle events
  events <- pollEvents
  case handleEvents previousCargo events of
    Nothing -> return () -- exit game.

    Just handledEventsCargo -> do
      -- update
      let (updatedCargo, remainingLag) = update previousCargo $ previousLag + elapsedTicks

      -- render
      render renderer bc updatedCargo currentTick

      -- loop
      gameLoop renderer bc updatedCargo currentTick remainingLag


handleEvents :: Cargo c => c -> [Event] -> Maybe c
handleEvents cargo events = eventFunction cargo $ events

update :: Cargo c => c -> W.Word32 -> (c, W.Word32)
update cargo lag = updateWhileLagging cargo lag
  where
    updateWhileLagging :: Cargo c => c -> W.Word32 -> (c, W.Word32)
    updateWhileLagging cargo lag
      | lag >= millisecondsPerUpdate =
        let updatedCargo = updateFunction cargo $ millisecondsPerUpdate
        in updateWhileLagging updatedCargo $ lag - millisecondsPerUpdate
      | otherwise =
        (cargo, lag)
