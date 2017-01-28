module Behaviour where

import qualified Graphics.UI.GLFW as GLFW

type Scancode = Int

data InputEvent = KeyEvent GLFW.Key Scancode GLFW.KeyState GLFW.ModifierKeys

data Behaviour = Behaviour { eventFun :: InputEvent -> IO ()
                           , renderFun :: IO ()
                           , cleanupFun :: IO ()
                           }
