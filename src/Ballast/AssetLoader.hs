module Ballast.AssetLoader(loadAssets) where

import           Ballast.Types
import           Control.Monad
import qualified Data.Map      as M
import           SDL
import           SDL.Image

loadAssets :: Renderer -> BallastConfig -> IO BallastConfig
loadAssets r bc = do
  -- load textures
  loadedTextures <- foldM (\m (k, fp) -> do
                              v <- loadTexture r fp
                              return $ M.insert k v m
                          ) M.empty $ bcTextureAssets bc

  print $ M.keys loadedTextures

  return $
    bc { bcLoadedTextures = loadedTextures }
