module Ballast (BallastConfig(..), Cargo(..), run) where

import           Control.Monad (unless)
import qualified Data.Map      as M
import qualified Data.Text     as T
import qualified Data.Word     as W
import           Linear
import           Linear.Affine
import           SDL
import           SDL.Time

class Cargo a where
  renderables :: a -> [Renderable]
  eventFunction ::  a -> [Event] -> Maybe a
  updateFunction :: a -> W.Word32 -> a


data Renderable =
  Renderable { rPosition             :: V2 Int
             , rDimensions           :: V2 Int
             , rTexture              :: Texture
             , rRotation             :: Int -- degrees
             , rSpritePosition       :: V2 Int
             , rSpriteDimensions     :: V2 Int
             , rSpriteSize           :: V2 Int
             , rNofFrames            :: Int -- number of frames in the texture
             , rMillisecondsPerFrame :: Int
             }

data BallastConfig =
  BallastConfig { bcDimensions :: (Int, Int)
                , bcTitle      :: T.Text
                }


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

-- | Gameloop: http://gameprogrammingpatterns.com/game-loop.html'
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

      print remainingLag
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


render :: Cargo c => Renderer -> BallastConfig -> c -> W.Word32 -> IO ()
render renderer bc cargo currentTick = do
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  mapM_ (renderRenderable renderer currentTick) $ renderables cargo
  present renderer

renderRenderable :: Renderer -> W.Word32 -> Renderable -> IO ()
renderRenderable renderer currentTick renderable =
  copy renderer t (Just from) (Just to)
  where
    t = rTexture renderable
    frame = (fromIntegral currentTick)
            `div` (rMillisecondsPerFrame renderable)
            `mod` (rNofFrames renderable)

    (V2 spriteWidth _) = rSpriteDimensions renderable
    spritePositionOffset = fmap fromIntegral
                           $ V2 (spriteWidth * frame) 1
                           * (rSpritePosition renderable)
    from = Rectangle (P spritePositionOffset) $ fmap fromIntegral $ rDimensions renderable
    to   = Rectangle (P $ fmap fromIntegral (rPosition renderable)) $ fmap fromIntegral $ rDimensions renderable
