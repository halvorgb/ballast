module Ballast.Render(render) where

import           Ballast.Types
import qualified Data.Word     as W
import qualified Data.Map as M
import           Linear
import           Linear.Affine
import           SDL


render :: Cargo c => Renderer -> BallastConfig -> c -> W.Word32 -> IO ()
render renderer bc cargo currentTick = do
  rendererDrawColor renderer $= bcClearColor bc
  clear renderer
  mapM_ (renderRenderable textureMap renderer currentTick) $ renderables cargo
  present renderer

  where
    textureMap = bcLoadedTextures bc

-- | Renders a renderable sprite or renderable.
-- | Supports animations, several frames in one texture, they must be aligned in a rectangle.
-- | Growing along the x axis first.
renderRenderable :: M.Map TextureId Texture -> Renderer -> W.Word32 -> Renderable -> IO ()
renderRenderable textureMap renderer currentTick renderable =
  copy renderer texture (Just from) (Just to)
  where
    texture = maybe (error $ "Missing texture" ++ show (rTextureId renderable)) id $
              M.lookup (rTextureId renderable) textureMap

    spriteData = rSpriteData renderable

    (V2 nofSpritesW _) = spSpriteRect spriteData
    (V2 spriteWidth spriteHeight) = spSpriteDimensions spriteData

    frameNumber :: Int
    frameNumber = fromIntegral currentTick
                  `div` rMillisecondsPerFrame renderable
                  `mod` spNofSprites spriteData

    frame :: V2 Int
    frame = V2
            (frameNumber `mod` nofSpritesW)
            (frameNumber `div` nofSpritesW)


    spritePositionOffset = fmap fromIntegral
                           $ spSpritePosition spriteData
                           + frame * spSpriteDimensions spriteData

    from = Rectangle (P spritePositionOffset)
           $ fromIntegral <$> spSpriteDimensions spriteData

    to   = Rectangle (P $ fromIntegral <$> rPosition renderable)
           $ fromIntegral <$> rDimensions renderable
