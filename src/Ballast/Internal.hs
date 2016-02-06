module Ballast.Internal (BallastState(..)) where

import           Ballast.Types
import qualified Data.Map      as M
import           SDL

data BallastState =
  BallastState { bsLoadedTextures :: M.Map TextureId Texture
               , bsBallastConfig  :: BallastConfig
               }
