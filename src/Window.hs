module Window (
    withWindow
) where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Exception
import Control.Monad

withWindow :: (Window -> IO a)-> (Window -> a -> IO ()) -> (a -> IO ()) -> IO ()
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
                                    bracket (setup wnd) cleanup (mainLoop wnd)
                                 `finally` destroyWindow wnd
          mainLoop wnd stuff = do
              GL.clear [GL.ColorBuffer, GL.DepthBuffer]
              pollEvents
              action wnd stuff
              swapBuffers wnd
              close <- windowShouldClose wnd
              unless close $ mainLoop wnd stuff