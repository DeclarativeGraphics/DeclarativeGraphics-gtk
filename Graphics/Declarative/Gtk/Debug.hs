module Graphics.Declarative.Gtk.Debug where

import Graphics.Declarative.Physical2D
import Graphics.Declarative.Bordered as Bordered
import Graphics.Declarative.Cairo.Form as Form
import Graphics.Declarative.Cairo.Debug as Debug
import Graphics.Declarative.Cairo.TangoColors as Colors

import Graphics.Declarative.Gtk.Window as Window

import Data.Vec2 as Vec2

-- HACK
debugTangentProgram :: DebuggedForm -> IO ()
debugTangentProgram debugform
  = runFormProgram (0, 0) (moveForm debugform) $ \ event oldForm -> case event of
      MouseMove (x,y) -> let direction = Vec2.normalize (x-200, y-200)
                             newForm = moveForm $ debugAll direction $ debugform
                         in return (newForm, debuggedForm newForm)
      _               -> return (oldForm, debuggedForm oldForm)
  where
    moveForm = onDebug (move (200, 200))
    debugAll dir = debugTangent dir . debugTangentVector dir
