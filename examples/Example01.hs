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

textures = [("", "")]

config = BallastConfig { bcDimensions = (640, 480)
                       , bcTitle = "Hello World"
                       , bcTextures = textures
                       , bcClearColor = V4 140 130 137 255
                       , bcMsPerUpdate = 16
                       }

data Universe =
  Universe { something :: T.Text
           , someValue :: Int
           , seed      :: StdGen
           } deriving Show

instance Cargo Universe where
  renderables universe = []

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
    universe { someValue = randomValue, seed = newSeed }
    where
      (randomValue, newSeed) = random $ seed universe

initialSeed = 1
initialUniverse = Universe "something" 0 $ mkStdGen initialSeed

main :: IO ()
main = run config initialUniverse
