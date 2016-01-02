module Ballast (BallastConfig(..), Cargo(..), run) where

import           Control.Monad (unless)
import qualified Data.Map      as M
import qualified Data.Text     as T
import qualified Data.Word     as W
import           Linear        (V4 (..))
import           SDL
import           SDL.Time

class Cargo a where
  spriteMap :: a -> M.Map String Surface
  eventFunction ::  a -> [Event] -> Maybe a
  updateFunction :: a -> W.Word32 -> a

data BallastConfig =
  BallastConfig { dimensions :: (Int, Int)
                , title      :: T.Text
                }


-- 1000/60 = 16.666666 --> slightly faster than 60 fps
millisecondsPerUpdate :: W.Word32
millisecondsPerUpdate = 16

run :: Cargo c => BallastConfig -> c -> IO ()
run bc cargo = do
  initializeAll
  window <- createWindow (title bc) defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer

  ts <- ticks
  let lag = 0
  gameLoop renderer bc cargo ts lag

  -- do we need to deinitialize?

-- | Gameloop: http://gameprogrammingpatterns.com/game-loop.html'
--  ticks = milliseconds since initialization, elapedTicks is the delay (delta) inbetween each iteration.
gameLoop :: Cargo c => Renderer -> BallastConfig -> c -> W.Word32 -> W.Word32 -> IO ()
gameLoop renderer bc previousCargo previousTicks previousLag = do
  -- Poll ticks.
  currentTicks <- ticks
  let elapsedTicks = currentTicks - previousTicks

  -- poll and handle events
  events <- pollEvents
  case handleEvents previousCargo events of
    Nothing -> return () -- exit game.

    Just handledEventsCargo -> do
      -- update
      let (updatedCargo, remainingLag) = update previousCargo $ previousLag + elapsedTicks
 --updateWhileLagging previousCargo $ previousLag + elapsedTicks

      -- render
      render renderer bc updatedCargo

      -- loop
      gameLoop renderer bc updatedCargo currentTicks remainingLag

  -- where
  --   eventIsQPress event =
  --     case eventPayload event of
  --       KeyboardEvent keyboardEvent ->
  --         keyboardEventKeyMotion keyboardEvent == Pressed &&
  --         keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
  --       _ -> False

  --   qPressed events = not (null (filter eventIsQPress events))



handleEvents :: Cargo c => c -> [Event] -> Maybe c
handleEvents cargo events = eventFunction cargo $ events

update :: Cargo c => c -> W.Word32 -> (c, W.Word32)
update cargo lag = updateWhileLagging cargo lag
  where
    updateWhileLagging :: Cargo c => c -> W.Word32 -> (c, W.Word32)
    updateWhileLagging cargo lag
      | lag >= millisecondsPerUpdate =
        updateWhileLagging cargo $ lag - millisecondsPerUpdate

      | otherwise =
        (cargo, lag)

render :: Cargo c => Renderer -> BallastConfig -> c -> IO ()
render renderer cargo bc = do
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  present renderer
