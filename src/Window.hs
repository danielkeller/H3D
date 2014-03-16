module Window (
    withWindow,
    fbSize
) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Control.Exception
import Control.Monad
import Control.Wire hiding (when, (.), unless)

import Util

resizeCB :: WindowSizeCallback
resizeCB _ w h = viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))

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
                                    cullFace $= Just Back
                                    depthFunc $= Just Less
                                    bracket (setup wnd) cleanup (mainLoop wnd clockSession_ . action wnd)
                                 `finally` destroyWindow wnd
          mainLoop wnd s wire = do
              clear [ColorBuffer, DepthBuffer]
              pollEvents
              (ds, s') <- stepSession s
              (_, wire') <- stepWire wire ds (Right ())
              errs <- get errors
              unless (null errs) (print errs)
              swapBuffers wnd
              close <- windowShouldClose wnd
              unless close $ mainLoop wnd s' wire'