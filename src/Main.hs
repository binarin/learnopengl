module Main where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.GL.Core33 as GL

import System.IO
import System.Exit

bool :: Bool -> a -> a -> a
bool b falseRes trueRes = if b then trueRes else falseRes

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m nothingRes f = case m of
    Nothing -> nothingRes
    Just x  -> f x

errorCb err desc = do
  putStrLn desc

main :: IO ()
main = do
  GLFW.setErrorCallback $ Just errorCb
  successfulInit <- GLFW.init
  bool successfulInit exitFailure $ do
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
    GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
    GLFW.windowHint $ GLFW.WindowHint'Resizable False
    mw <- GLFW.createWindow 800 600 "LearnOpenGL" Nothing Nothing
    maybe' mw (GLFW.terminate >> exitFailure) $ \window -> do
      GLFW.makeContextCurrent mw
      (width, height) <- GLFW.getFramebufferSize window
      GL.glViewport (fromIntegral width) (fromIntegral height) 0 0
      mainLoop window
      GLFW.destroyWindow window
      exitSuccess

mainLoop window = do
  shouldClose <- GLFW.windowShouldClose window
  case shouldClose of
    True ->
      return ()
    False -> do
      GLFW.pollEvents
      GLFW.swapBuffers window
      mainLoop window
