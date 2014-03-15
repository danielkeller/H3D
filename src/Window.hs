module Window (
    withWindow,
    fbSize
) where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Exception
import Control.Monad
import Control.Wire hiding (when, (.), unless)

import Util

resizeCB :: WindowSizeCallback
resizeCB _ w h = GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))

fbSize :: Floating a => Window -> PlainWire (a, a)
fbSize wnd = (fint *** fint) <<< (mkGen_ $ const $ fmap Right $ getFramebufferSize wnd)
    where fint = arr fromIntegral

withWindow :: (Window -> IO a)
               -> (Window -> a -> PlainWire b)
               -> (a -> IO ()) -> IO ()
withWindow setup action cleanup = 
    do res <- GLFW.init
       setErrorCallback (Just (const error))
       windowHint (WindowHint'ContextVersionMajor 3)
       windowHint (WindowHint'ContextVersionMinor 3)
       windowHint (WindowHint'OpenGLProfile OpenGLProfile'Core)
       when res run
    `finally` terminate
    where run =
              do w <- createWindow 1024 768 "Hello Triangle" Nothing Nothing
                 case w of
                     Nothing -> return ()
                     Just wnd -> do makeContextCurrent w
                                    setWindowSizeCallback wnd (Just resizeCB)
                                    GL.cullFace GL.$= Just GL.Back
                                    --set the clear color to a pleasant warm off-black
                                    GL.clearColor GL.$= (GL.Color4 0.28 0.25 0.25 1)
                                    bracket (setup wnd) cleanup (mainLoop wnd clockSession_ . action wnd)
                                 `finally` destroyWindow wnd
          mainLoop wnd s wire = do
              GL.clear [GL.ColorBuffer, GL.DepthBuffer]
              pollEvents
              (ds, s') <- stepSession s
              (_, wire') <- stepWire wire ds (Right ())
              errs <- GL.get GL.errors
              unless (null errs) (print errs)
              swapBuffers wnd
              close <- windowShouldClose wnd
              unless close $ mainLoop wnd s' wire'