module Graphics.Declarative.Gtk.KeyboardInput where

import System.Glib.UTFString (glibToString)
import Debug.Trace

import qualified Graphics.UI.Gtk as G

data Key = Letter Char
         | Special SpecialKey
           deriving (Show, Eq, Ord)

data SpecialKey = Return | Escape | Backspace | Delete | Insert | Shift
                | ArrLeft | ArrUp | ArrRight | ArrDown
                | Home | End | PageUp | PageDown
                | DeadCircumflex | DeadGrave | DeadAcute
                | F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8 | F9 | F10 | F11 | F12
                | Control | ControlRight | ShiftRight
                | CapsLock | Tab | Alt | AltRight
                | AltGr | Menu
                deriving (Show, Eq, Ord)


keyboardInputFromGdk :: G.KeyVal -> Maybe Key
keyboardInputFromGdk k = maybe parseSpecialKey parseLetter (G.keyToChar k)
  where
    parseLetter c = Just $ Letter c
    parseSpecialKey = case glibToString $ G.keyName k of
      "Return"          -> Just $ Special Return
      "Escape"          -> Just $ Special Escape
      "BackSpace"       -> Just $ Special Backspace
      "Delete"          -> Just $ Special Delete
      "Insert"          -> Just $ Special Insert
      "Shift_L"         -> Just $ Special Shift
      "Left"            -> Just $ Special ArrLeft
      "Right"           -> Just $ Special ArrRight
      "Down"            -> Just $ Special ArrDown
      "Up"              -> Just $ Special ArrUp
      "Home"            -> Just $ Special Home
      "End"             -> Just $ Special End
      "Page_Up"         -> Just $ Special PageUp
      "Page_Down"       -> Just $ Special PageDown
      "dead_circumflex" -> Just $ Special DeadCircumflex
      "dead_grave"      -> Just $ Special DeadGrave
      "dead_acute"      -> Just $ Special DeadAcute
      "F1"              -> Just $ Special F1
      "F2"              -> Just $ Special F2
      "F3"              -> Just $ Special F3
      "F4"              -> Just $ Special F4
      "F5"              -> Just $ Special F5
      "F6"              -> Just $ Special F6
      "F7"              -> Just $ Special F7
      "F8"              -> Just $ Special F8
      "F9"              -> Just $ Special F9
      "F10"             -> Just $ Special F10
      "F11"             -> Just $ Special F11
      "F12"             -> Just $ Special F12
      "Control_L"       -> Just $ Special Control
      "Control_R"       -> Just $ Special ControlRight
      "Shift_R"         -> Just $ Special ShiftRight
      "Caps_Lock"       -> Just $ Special CapsLock
      "Tab"             -> Just $ Special Tab
      "Alt_L"           -> Just $ Special Alt
      "Alt_R"           -> Just $ Special AltRight
      "Menu"            -> Just $ Special Menu
      "ISO_Level3_Shift"-> Just $ Special AltGr
      unknown     -> (trace ("Don't know how to interpret GDK key " ++ show unknown) Nothing)
