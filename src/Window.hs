{-# LANGUAGE DataKinds, TypeOperators #-}
module Window (
    withWindow,
    fbSize
) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class(lift)
import Control.Arrow
import Control.Wire(mkGen_, Timed(..), stepWire)
import Data.Time
import Data.Vinyl

import Util
import Components
import Loader

resizeCB :: WindowSizeCallback
resizeCB _ w h = viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))

fbSize :: Floating a => Window -> PlainWire0 GameM (a, a)
fbSize wnd = (fint *** fint) <<< (mkGen_ $ const $ fmap Right $ lift (getFramebufferSize wnd))
    where fint = arr fromIntegral

withWindow :: (Window -> IO ())
               -> (Window -> Component '[] '[DrawScene])
               -> IO ()
withWindow setup action = 
    do res <- GLFW.init
       setErrorCallback (Just (const error))
       windowHint (WindowHint'ContextVersionMajor 3)
       windowHint (WindowHint'ContextVersionMinor 3)
       windowHint (WindowHint'OpenGLProfile OpenGLProfile'Core)
       when res (run =<< createWindow 1024 768 "λ 3D" Nothing Nothing)
    `finally` terminate
    where run Nothing = return ()
          run (Just wnd) = do
              makeContextCurrent (Just wnd)
              setWindowSizeCallback wnd (Just resizeCB)
              cullFace $= Just Back
              depthFunc $= Just Less
              back <- getCurrentTime
              setup wnd
              evalLoader $ mainLoop wnd back (addUTCTime dt back) (const (return ())) $ action wnd
            `finally` destroyWindow wnd

mainLoop :: Window -> UTCTime -> UTCTime -> DrawFun -> Component '[] '[DrawScene] -> Loader ()
mainLoop wnd back -- completed time
             front -- end of current tick
             drawfn -- current render action
             wire -- current physics state
  = do
    now <- lift getCurrentTime
    let back' = min (addUTCTime 0.25 back) now
    errs <- lift (get errors)
    unless (null errs) $ lift $ print errs
    close <- lift (windowShouldClose wnd)
    
    case () of
        --we have enough unsimulated time to need a new physics step
      () | back' >= front -> do
            lift pollEvents
            (drawfn', wire') <- stepWire wire (Timed dt ()) (if close then Left () else Right RNil) --inhibit on last frame
            let front' = addUTCTime dt back'
            --putStrLn $ "Sim " ++ show (utctDayTime back') ++ " -> " ++ show (utctDayTime front')
            case drawfn' of
                Right theDrawfn' -> mainLoop wnd back' front' (rGet drawScene theDrawfn') wire'
                Left _ -> return () --close when the wire inhbits
        --the current draw action is still fresh
         | otherwise -> do
            lift $ clear [ColorBuffer, DepthBuffer]
            --the alpha is the amount of the physics step we have left to display
            let alpha = 1 - (realToFrac $ front `diffUTCTime` back) / dt
            lift $ drawfn alpha
            lift $ swapBuffers wnd
            --putStrLn $ "Draw " ++ show (utctDayTime back') ++ " -> " ++ show (utctDayTime front)
            mainLoop wnd back' front drawfn wire

dt :: Fractional a => a
dt = 0.03