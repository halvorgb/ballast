{-# LANGUAGE OverloadedStrings #-}
module Example01 where

import           Ballast
import           Control.Monad
import qualified Data.Map      as M
import qualified Data.Text     as T
import qualified Data.Word     as W
import           Linear
import           SDL.Event
import           SDL.Input
import           System.Random

textureAssets = [("test1", "assets/sprite_test.png")]

config = BallastConfig { bcDimensions = (640, 480)
                       , bcTitle = "Hello World"
                       , bcTextureAssets = textureAssets
                       , bcClearColor = V4 140 130 137 255
                       , bcMsPerUpdate = 16
                       , bcLoadedTextures = M.empty
                       }

data Universe =
  Universe { something :: T.Text
           , someValue :: Int
           , seed      :: StdGen
           } deriving Show

instance Cargo Universe where
  renderables universe = [
    Renderable { rPosition = V2 200 200
               , rDimensions = V2 128 128
               , rTextureId = "test1"
               , rRotation = 0
               , rMillisecondsPerFrame = 100
               , rSpriteData =
                 SpriteData { spSpritePosition = V2 0 0
                            , spSpriteDimensions = V2 128 128
                            , spNofSprites = 10
                            , spSpriteRect = V2 6 10
                              }
               }
    ]

  eventFunction = foldM handleEvent
    where
      handleEvent :: Universe -> Event -> Maybe Universe
      handleEvent universe (Event _ (KeyboardEvent keyboardEvent))
        | keyboardEventKeyMotion keyboardEvent == Pressed &&
          keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ =
            Nothing
        | otherwise =
          Just universe

      handleEvent universe _ = Just universe

  updateFunction universe delta =
    universe { someValue = randomValue
             , seed = newSeed }
    where
      (randomValue, newSeed) = random $ seed universe

initialSeed = 1
initialUniverse = Universe "something" 0 $ mkStdGen initialSeed


main :: IO ()
main = run config initialUniverse
