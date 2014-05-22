{-# LANGUAGE DataKinds, TypeOperators #-}
module Window (
    withWindow,
    fbSize
) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Control.Exception
import Control.Monad
import Control.Arrow
import Control.Wire(mkGen_, Timed(..), stepWire)
import Data.Time
import Data.Vinyl

import Scene
import Util

resizeCB :: WindowSizeCallback
resizeCB _ w h = viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))

fbSize :: Floating a => Window -> PlainWire0 (a, a)
fbSize wnd = (fint *** fint) <<< (mkGen_ $ const $ fmap Right $ getFramebufferSize wnd)
    where fint = arr fromIntegral

withWindow :: (Window -> IO a)
               -> (Window -> a -> Component '[] '[DrawScene])
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
                                    back <- getCurrentTime
                                    bracket (setup wnd) cleanup (mainLoop wnd back (addUTCTime dt back)
                                                                          (const (return ())) . action wnd)
                                 `finally` destroyWindow wnd

mainLoop :: Window -> UTCTime -> UTCTime -> DrawFun -> Component '[] '[DrawScene] -> IO ()
mainLoop wnd back -- completed time
             front -- end of current tick
             drawfn -- current render action
             wire -- current physics state
  = do
    now <- getCurrentTime
    let back' = min (addUTCTime 0.25 back) now
    errs <- get errors
    unless (null errs) (print errs)
    close <- windowShouldClose wnd
    
    unless close $ case () of
        --we have enough unsimulated time to need a new physics step
      () | back' >= front -> do
            pollEvents
            (Right drawfn', wire') <- stepWire wire (Timed dt ()) (Right RNil)
            let front' = addUTCTime dt back'
            --putStrLn $ "Sim " ++ show (utctDayTime back') ++ " -> " ++ show (utctDayTime front')
            mainLoop wnd back' front' (rGet drawScene drawfn') wire'
        --the current draw action is still fresh
         | otherwise -> do
            clear [ColorBuffer, DepthBuffer]
            --the alpha is the amount of the physics step we have left to display
            let alpha = 1 - (realToFrac $ front `diffUTCTime` back) / dt
            drawfn alpha
            swapBuffers wnd
            --putStrLn $ "Draw " ++ show (utctDayTime back') ++ " -> " ++ show (utctDayTime front)
            mainLoop wnd back' front drawfn wire

dt :: Fractional a => a
dt = 0.03