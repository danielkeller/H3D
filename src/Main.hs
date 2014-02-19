{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
module Main (
	main
) where

import qualified Graphics.UI.GLFW as GLFW
import Linear
import Data.Vinyl

import Window
import Object

keyCB :: GLFW.KeyCallback
keyCB wnd GLFW.Key'Escape _ _ _ = GLFW.setWindowShouldClose wnd True
keyCB _ _ _ _ _ = return ()
--keyCB _ key scan state mods = return ()

main :: IO ()
main = withWindow setup action cleanup
    where setup wnd = do GLFW.setKeyCallback wnd (Just keyCB)
                         loadObject "teapot.obj"
          action _ teapot = drawObject (mkTransformationMat (eye3 !!* 0.2) (V3 0 0 0)) $
                                objRec =: teapot <+> objXfrm =: eye4
          cleanup teapot = freeObject teapot
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