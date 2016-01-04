module Ballast.Render(render) where

import           Ballast.Types
import qualified Data.Word     as W
import           Linear
import           Linear.Affine
import           SDL


render :: Cargo c => Renderer -> BallastConfig -> c -> W.Word32 -> IO ()
render renderer bc cargo currentTick = do
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  mapM_ (renderRenderable renderer currentTick) $ renderables cargo
  present renderer

-- | Renders a renderable sprite or renderable.
-- | Supports animations, several frames in one texture, they must be aligned in a rectangle.
-- | Growing along the x axis first.
renderRenderable :: Renderer -> W.Word32 -> Renderable -> IO ()
renderRenderable renderer currentTick renderable =
  copy renderer t (Just from) (Just to)
  where
    t = rTexture renderable

    (V2 nofSpritesW _) = rAnimations renderable
    (V2 spriteWidth spriteHeight) = rSpriteDimensions renderable

    frameNumber :: Int
    frameNumber = fromIntegral currentTick
                  `div` rMillisecondsPerFrame renderable
                  `mod` rNofSprites renderable

    frame :: V2 Int
    frame = V2
            (frameNumber `mod` nofSpritesW)
            (frameNumber `div` nofSpritesW)


    spritePositionOffset = fmap fromIntegral
                           $ rSpritePosition renderable
                           + frame * rSpriteDimensions renderable

    from = Rectangle (P spritePositionOffset)
           $ fromIntegral <$> rSpriteDimensions renderable

    to   = Rectangle (P $ fromIntegral <$> rPosition renderable)
           $ fromIntegral <$> rDimensions renderable
