module Graphics.Declarative.Gtk.Window where

import Control.Applicative
import Data.IORef

import qualified Graphics.UI.Gtk as G
import Graphics.UI.Gtk (AttrOp (..) -- for :=
                       ,on
                       )
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.UI.Gtk.Gdk.EventM as E
import Graphics.Rendering.Cairo (liftIO)

import Graphics.Declarative.Bordered as Bordered
import Graphics.Declarative.Cairo.Form as Form
import Graphics.Declarative.Physical2D

import Graphics.Declarative.Gtk.KeyboardInput



data GtkEvent = Expose
              | Resize (Int, Int)
              | Tick
              | KeyPress Key
              | KeyRelease Key
              | MouseMove (Double, Double)
              deriving (Show, Eq)


type Renderer = Double -> Double -> Cairo.Render ()


showFormWindow :: (Double, Double) -> Form -> IO ()
showFormWindow origin = showCairoWindow . formRenderer origin

showCairoWindow :: Renderer -> IO ()
showCairoWindow renderer = runCairoProgram () step
  where step _event state = return (state, renderer)

runFormProgram :: (Double, Double)
               -> state
               -> (GtkEvent -> state -> IO (state, Form))
               -> IO ()
runFormProgram origin state step
  = runCairoProgram state $ \ event state -> do
      (newState, newForm) <- step event state
      return (newState, formRenderer origin newForm)

runCairoProgram :: state
                -> (GtkEvent -> state -> IO (state, Renderer))
                -> IO ()
runCairoProgram state step = gtkWindowCanvas $ \canvas -> do
  stateRef <- newIORef state

  let processEvent :: GtkEvent -> IO ()
      processEvent event = do
        state <- readIORef stateRef
        (newstate, renderer) <- step event state
        writeIORef stateRef newstate
        renderDoubleBuffered canvas renderer

      maybeProcessEvent :: Maybe GtkEvent -> E.EventM any ()
      maybeProcessEvent = maybe (return ()) (liftIO . processEvent)

      gtkProcessEvent :: E.EventM i (Maybe GtkEvent) -> E.EventM i Bool
      gtkProcessEvent handler = handler >>= maybeProcessEvent >> E.eventSent

  G.timeoutAdd (processEvent Tick >> return True) 20 --ms

  canvas `G.widgetAddEvents` [G.PointerMotionMask]

  canvas `on` G.exposeEvent       $ gtkProcessEvent handleExpose
  canvas `on` G.configureEvent    $ gtkProcessEvent handleConfigure
  canvas `on` G.keyPressEvent     $ gtkProcessEvent handleKeyPress
  canvas `on` G.keyReleaseEvent   $ gtkProcessEvent handleKeyRelease
  canvas `on` G.motionNotifyEvent $ gtkProcessEvent handleMouseMove

handleExpose :: E.EventM E.EExpose (Maybe GtkEvent)
handleExpose = return (Just Expose)

handleConfigure :: E.EventM E.EConfigure (Maybe GtkEvent)
handleConfigure = do
  size <- E.eventSize
  return $ Just $ Resize size

handleKeyPress :: E.EventM E.EKey (Maybe GtkEvent)
handleKeyPress = do
  key <- E.eventKeyVal
  return $ KeyPress <$> keyboardInputFromGdk key

handleKeyRelease :: E.EventM E.EKey (Maybe GtkEvent)
handleKeyRelease = do
  key <- E.eventKeyVal
  return $ KeyRelease <$> keyboardInputFromGdk key


handleMouseMove :: E.EventM E.EMotion (Maybe GtkEvent)
handleMouseMove = do
  pos <- E.eventCoordinates
  return $ Just (MouseMove pos)


-------------------- RENDERING ------------------------

formRenderer :: (Double, Double) -> Form -> Renderer
formRenderer (originx,originy) form w h
  = Form.drawForm $ move (originx * w, originy * h) $ form


renderForm :: G.WidgetClass w => w -> (Double, Double) -> Form -> IO ()
renderForm canvas origin form
  = renderDoubleBuffered canvas (formRenderer origin form)



--- GTK ---
gtkWindowCanvas f = do
  G.initGUI

  window <- G.windowNew
  screen <- G.windowGetScreen window
  w <- G.screenGetWidth screen
  h <- G.screenGetHeight screen
  G.set window [G.windowDefaultWidth   := (ceiling $ 0.7 * fromIntegral w)
               ,G.windowDefaultHeight  := (ceiling $ 0.7 * fromIntegral h)
               ,G.windowWindowPosition := G.WinPosCenter
               ]

  canvas <- G.drawingAreaNew
  G.containerAdd window canvas

  G.set canvas [G.widgetCanFocus := True]
  G.widgetModifyBg canvas G.StateNormal gtkWhite
  G.widgetShowAll window

  G.onDestroy window G.mainQuit

  f canvas

  G.mainGUI


gtkWhite = G.Color 65535 65535 65535


renderCairo :: G.WidgetClass w => w -> Renderer -> IO ()
renderCairo canvas renderer = do
  (w, h) <- G.widgetGetSize canvas
  drawWin <- G.widgetGetDrawWindow canvas
  G.renderWithDrawable drawWin (renderer (fromIntegral w) (fromIntegral h))


renderDoubleBuffered :: G.WidgetClass w => w -> Renderer -> IO ()
renderDoubleBuffered canvas renderer = renderCairo canvas renderOnSecondBuffer
  where
    renderOnSecondBuffer w h = do
      Cairo.pushGroup
      delete w h
      renderer w h
      Cairo.popGroupToSource
      Cairo.paint

    delete w h = do
      Cairo.setSourceRGB 1 1 1
      Cairo.rectangle 0 0 w h
      Cairo.fill
