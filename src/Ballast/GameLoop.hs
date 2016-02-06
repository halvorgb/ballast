module Ballast.GameLoop(gameLoop) where

import           Ballast.Internal
import           Ballast.Render
import           Ballast.Types
import qualified Data.Word        as W
import           Linear
import           Linear.Affine
import           SDL
import           SDL.Time

-- | Gameloop: http://gameprogrammingpatterns.com/game-loop.html
--  ticks = milliseconds since initialization, elapedTicks is the delay (delta) inbetween each iteration.
gameLoop :: (Show c, Cargo c) => Renderer -> BallastState -> c -> W.Word32 -> W.Word32 -> IO ()
gameLoop renderer bs previousCargo previousTick previousLag = do
  -- Poll ticks.
  currentTick <- ticks
  let elapsedTicks = currentTick - previousTick

  -- poll and handle events
  events <- pollEvents
  case handleEvents previousCargo events of
    Nothing -> return () -- exit game.

    Just handledEventsCargo -> do
      -- update
      let (updatedCargo, remainingLag) = update previousCargo bs $ previousLag + elapsedTicks
      --print remainingLag
      --print updatedCargo
      -- render
      render renderer bs updatedCargo currentTick

      -- loop
      gameLoop renderer bs updatedCargo currentTick remainingLag


handleEvents :: Cargo c => c -> [Event] -> Maybe c
handleEvents = eventFunction

update :: Cargo c => c -> BallastState -> W.Word32 -> (c, W.Word32)
update c bs l = updateWhileLagging c l
  where
    bc = bsBallastConfig bs

    updateWhileLagging :: Cargo c => c -> W.Word32 -> (c, W.Word32)
    updateWhileLagging cargo lag
      | lag >= millisecondsPerUpdate =
        let updatedCargo = updateFunction cargo millisecondsPerUpdate
        in updateWhileLagging updatedCargo $ lag - millisecondsPerUpdate
      | otherwise =
        (cargo, lag)

    millisecondsPerUpdate = bcMsPerUpdate bc
