-- | Graphics facade
module Graphics (
    module Graphics.Rendering.OpenGL,
    module Graphics.GLUtil,
    module Graphics.VinylGL,
    module Linear.GL,
) where


import Graphics.Rendering.OpenGL hiding (Shader, Uniform)
import Graphics.GLUtil
import Graphics.VinylGL hiding (setUniforms, setAllUniforms)
import Linear.GL