module Ballast.Types(Cargo(..), Renderable(..), BallastConfig(..)) where

import qualified Data.Text as T
import qualified Data.Word as W
import           Linear
import           SDL

class Cargo a where
  renderables :: a -> [Renderable]
  eventFunction ::  a -> [Event] -> Maybe a
  updateFunction :: a -> W.Word32 -> a


data Renderable =
  Renderable { rPosition             :: V2 Int -- position on the screen.
             , rDimensions           :: V2 Int
             , rTexture              :: Texture
             , rRotation             :: Int -- degrees
             , rSpritePosition       :: V2 Int -- position of the sprite, within texture
             , rSpriteDimensions     :: V2 Int
             , rNofSprites           :: Int
             , rAnimations           :: V2 Int -- a rectangle of frames.
             , rMillisecondsPerFrame :: Int
             }

data BallastConfig =
  BallastConfig { bcDimensions :: (Int, Int)
                , bcTitle      :: T.Text
                , bcTextures   :: [(T.Text, FilePath)]
                }
