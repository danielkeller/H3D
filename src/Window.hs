{-# LANGUAGE BangPatterns #-}
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
import Control.Concurrent
import Control.Concurrent.MVar

import Util

resizeCB :: WindowSizeCallback
resizeCB _ w h = viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))

fbSize :: Floating a => Window -> PlainWire (a, a)
fbSize wnd = (fint *** fint) <<< (mkGen_ $ const $ fmap Right $ getFramebufferSize wnd)
    where fint = arr fromIntegral

withWindow :: (Window -> IO a)
               -> (Window -> a -> PlainWire DrawFun)
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
                                    mvdraw <- newEmptyMVar
                                    accum <- getCurrentTime
                                    bracket (setup wnd) cleanup (\resources -> do 
                                        forkOS $ makeContextCurrent w >> drawLoop mvdraw wnd
                                        physLoop wnd accum mvdraw (action wnd resources)) --has to run in main thread
                                 `finally` destroyWindow wnd

--Function to draw the frame, and time that frame starts
data DrawData = DrawData DrawFun UTCTime

drawLoop :: MVar DrawData -> Window -> IO ()
drawLoop mvdraw wnd = do
    --wait for physics to finish
    DrawData draw accum <- takeMVar mvdraw
    let loop = do
          now <- getCurrentTime
          let !alpha = realToFrac (diffUTCTime now accum) / dt
          putStr "draw "
          print alpha
          --the current draw action is still fresh
          when (alpha <= 1) $ do
              clear [ColorBuffer, DepthBuffer]
              --draw alpha
              swapBuffers wnd
              errs <- get errors
              unless (null errs) (print errs)
              loop
    loop
    close <- windowShouldClose wnd
    unless close (drawLoop mvdraw wnd)

--ASCII art code is best code
physLoop :: Window -> UTCTime -> MVar DrawData -> PlainWire DrawFun -> IO ()
physLoop wnd accum mvdraw wire = do { {-
+-+ <- accum
|#|
|#| )- previous frame physics
|#|
+-+ -} now <- getCurrentTime;
       putStr "delay "; print (diffUTCTime accum' now * 1000000); {-
  | -} when (now < accum') (threadDelay (truncate (diffUTCTime accum' now * 1000000))); {- in usec
  |    waiting for this frame'e events to happen...
  |
  |
+-+ <- accum'
|#| -} pollEvents; {- at least dt has passed, so get the last dt's worth of events 
|#| -} (Right !draw, wire') <- stepWire wire (Timed dt ()) (Right ()); putStr "tick ... ";{-
|#| running this frame's physics sim...
|#|
|#| -} putMVar mvdraw (DrawData draw accum'); putStrLn "sent"; {- done, send it over!
+-+ -} physLoop wnd accum' mvdraw wire'; -- loop into next frame
       } where accum' = addUTCTime dt accum

{-
physLoop' :: Window -> UTCTime -> UTCTime -> MVar DrawData -> PlainWire DrawFun -> IO ()
physLoop' wnd back -- completed time
             front -- end of current tick
             draw -- current render action
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
            (Right draw', wire') <- stepWire wire (Timed dt ()) (Right ())
            let front' = addUTCTime dt back'
            --putStrLn $ "Sim  " ++ show (utctDayTime back') ++ " -> " ++ show (utctDayTime front')
            physLoop' wnd back' front' draw' wire'
         | otherwise -> do
            clear [ColorBuffer, DepthBuffer]
            --the alpha is the amount of the physics step we have left to display
            let alpha = 1 - (realToFrac $ front `diffUTCTime` back) / dt
            draw alpha
            swapBuffers wnd
            --putStrLn $ "Draw " ++ show (utctDayTime back') ++ " -> " ++ show (utctDayTime front)
            physLoop wnd back' front draw wire
-}
dt :: Fractional a => a
dt = 0.03