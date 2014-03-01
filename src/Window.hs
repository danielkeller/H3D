module Window (
    withWindow
) where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Exception
import Control.Monad
import Control.Wire hiding (when, (.), unless)

import Util

resizeCB :: WindowSizeCallback
resizeCB _ w h = GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))

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
                                    bracket (setup wnd) cleanup (mainLoop wnd clockSession_ . action wnd)
                                 `finally` destroyWindow wnd
          mainLoop wnd s wire = do
              GL.clear [GL.ColorBuffer, GL.DepthBuffer]
              pollEvents
              (ds, s') <- stepSession s
              (_, wire') <- stepWire wire ds (Right ())
              swapBuffers wnd
              close <- windowShouldClose wnd
              unless close $ mainLoop wnd s' wire'