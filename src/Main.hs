{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
module Main (
	main
) where

import qualified Graphics.UI.GLFW as GLFW
import Control.Exception
import Control.Monad

keyCB :: GLFW.KeyCallback
keyCB wnd GLFW.Key'Escape _ _ _ = GLFW.setWindowShouldClose wnd True
keyCB _ key scan state mods = return ()

main :: IO ()
main = withGLFW $ withWindow $ mainLoop
    where mainLoop :: GLFW.Window -> IO ()
          mainLoop wnd = do
              GLFW.pollEvents
              GLFW.swapBuffers wnd
              close <- GLFW.windowShouldClose wnd
              unless close $ mainLoop wnd
          withGLFW act =
              do res <- GLFW.init
                 when res act
              `finally` GLFW.terminate
          withWindow act =
              do wnd <- GLFW.createWindow 1024 768 "Hello Triangle" Nothing Nothing
                 case wnd of
                     Nothing -> return ()
                     Just w -> do GLFW.makeContextCurrent wnd
                                  GLFW.setKeyCallback w (Just keyCB)
                                  act w
                               `finally` GLFW.destroyWindow w

--for another day
{-
import TestAPI
import Data.Typeable(Typeable)

deriving instance Typeable Callbacks

cbs = Callbacks {foo = putStrLn}

main :: IO ()
main = do
	res <- runInterpreter (do
		set [searchPath := ["api", "script"]]
		loadModules ["Script"]
		setImportsQ [("Prelude", Nothing), ("Script", Just "Script"), ("TestAPI", Nothing)]
		interpret "Script.script" (undefined :: Script)
		)
	case res of
		Left err -> print err
		Right script -> script cbs
-}