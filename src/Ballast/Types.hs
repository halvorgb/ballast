module Ballast.Types(Cargo(..), Renderable(..), SpriteData(..), BallastConfig(..), TextureId, ) where

import qualified Data.Map  as M
import qualified Data.Text as T
import qualified Data.Word as W
import           Linear
import           SDL

type TextureId = T.Text

class Cargo a where
  renderables :: a -> [Renderable]
  eventFunction ::  a -> [Event] -> Maybe a
  updateFunction :: a -> W.Word32 -> a


data Renderable =
  Renderable { rPosition             :: V2 Int -- position on the screen.
             , rDimensions           :: V2 Int
             , rTextureId            :: TextureId
             , rRotation             :: Int -- degrees
             , rMillisecondsPerFrame :: Int
             , rSpriteData           :: SpriteData
             }

data SpriteData =
  SpriteData { spSpritePosition   :: V2 Int -- position of the sprite, within texture
             , spSpriteDimensions :: V2 Int
             , spNofSprites       :: Int
             , spSpriteRect       :: V2 Int-- a rectangle of frames.
             }

data BallastConfig =
  BallastConfig { bcDimensions     :: (Int, Int)
                , bcTitle          :: T.Text
                , bcTextureAssets  :: [(TextureId, FilePath)]
                , bcClearColor     :: V4 W.Word8
                , bcMsPerUpdate    :: W.Word32
                , bcLoadedTextures :: M.Map TextureId Texture
                }
