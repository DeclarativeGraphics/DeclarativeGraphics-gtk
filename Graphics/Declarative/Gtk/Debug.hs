module Graphics.Declarative.Gtk.Debug where

import Graphics.Declarative.Bordered as Bordered
import Graphics.Declarative.Cairo.Form as Form
import Graphics.Declarative.Cairo.Debug as Debug
import Graphics.Declarative.Cairo.TangoColors as Colors

import Graphics.Declarative.Gtk.Window as Window

import Data.Vec2 as Vec2

-- HACK
debugTangentProgram :: Form -> IO ()
debugTangentProgram form
  = runFormProgram (0, 0) (move (200, 200) form) $ \ event oldForm -> case event of
      MouseMove (x,y) -> let direction = Vec2.normalize (x-200, y-200)
                             newForm = move (200, 200) $ debugTangent direction form
                         in return (newForm, newForm)
      _               -> return (oldForm, oldForm)
