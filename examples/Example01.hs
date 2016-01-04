{-# LANGUAGE OverloadedStrings #-}
module Example01 where

import           Ballast
import           Control.Monad
import qualified Data.Map      as M
import qualified Data.Text     as T
import qualified Data.Word     as W
import           SDL.Event
import           SDL.Input
import           System.Random

textures = [("", "")]

config = BallastConfig { bcDimensions = (640, 480)
                       , bcTitle = "Hello World"
                       , bcTextures = textures
                       }

data Universe =
  Universe { something :: T.Text
           , seed      :: StdGen
           }

instance Cargo Universe where
  renderables universe = []

  eventFunction universe events = foldM handleEvent universe events
    where
      handleEvent :: Universe -> Event -> Maybe Universe
      handleEvent universe (Event _ (KeyboardEvent keyboardEvent))
        | keyboardEventKeyMotion keyboardEvent == Pressed &&
          keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ =
            Nothing
        | otherwise =
          Just universe

      handleEvent universe _ = Just universe

  updateFunction universe delta
    | something universe == "somethang" = universe
    | otherwise = universe


initialSeed = 1
initialUniverse = Universe "something" $ mkStdGen initialSeed

main :: IO ()
main = run config initialUniverse
