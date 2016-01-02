{-# LANGUAGE OverloadedStrings #-}
module Example01 where

import           Ballast
import qualified Data.Map      as M
import qualified Data.Text     as T
import qualified Data.Word     as W
import           SDL.Event
import           System.Random

config = BallastConfig (640, 480) "Hello World"

data Universe =
  Universe { something :: T.Text
           , seed      :: StdGen
           }

instance Cargo Universe where
  spriteMap u = M.empty

  updateFunction universe events = universe

  updateFunction universe delta
    | something universe == "somethang" = universe
    | otherwise = universe

initialSeed = 1
initialUniverse = Universe "something" $ mkStdGen initialSeed

main :: IO ()
main = run config initialUniverse
