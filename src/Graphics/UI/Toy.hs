{-# LANGUAGE MultiParamTypeClasses,
             TypeFamilies,
             TupleSections,
             ViewPatterns
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Toy
-- Copyright   :  (c) 2012 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- This is a simple interface for making applications which use the mouse and
-- keyboard.  Implementors of this interface process the input events into more
-- palatable data types.  Currently, only the gtk-toy package implements it.
--
-----------------------------------------------------------------------------
module Graphics.UI.Toy
  ( MousePos, KeyModifier
  , KeyInfo, KeyTable, MouseEvent, KeyEvent, InputState(..)
  , Interactive(..)

  -- * InputState Accessors
  , keyInfo, keyHeld, mouseHeld

  -- * Utilities
  -- | Functions to allow for writing simpler, pure implementations of the
  --   different members of Interactive.
  , simpleTick, simpleMouse, simpleKeyboard

  -- * Key Handlers
  -- | Monadic convenience for building implementations of 'keyboard'.
  , KeyHandler, keyHandler
  , handleKeys
  ) where

import Control.Monad (when, liftM)
import Control.Monad.State (StateT, execStateT, get, put, lift)
import qualified Data.Map as M

type family MousePos b :: *
type family KeyModifier b :: *

-- | Information about the most recent key-state transition.
--   The tuple contains whether the button was pressed,
--   at what time in msec, and with which modifiers.
type KeyInfo b = (Bool, Int, [KeyModifier b])

-- | A map of keynames to last-received event regarding each respective key.
--   This can be interpreted as the current keyboard state - a key is down if
--   it was last seen being pressed.
type KeyTable b = M.Map String (KeyInfo b)

data InputState b = InputState 
  { mousePos :: MousePos b -- ^ The most recent mouse position.
  , keyTable :: KeyTable b -- ^ Map from key-name to most recent event.
  }

-- | A @KeyEvent@ tuple specifies whether the key was pressed or not, and
--   which key was pressed.  @Right Char@ is yielded for keys which would
--   normally correspond to character insertions, while @Left String@ provides
--   GTK-convention names for the rest.
type KeyEvent = (Bool, Either String Char)

-- | A @MouseEvent@ is 'Nothing' if it's a mouse motion event, and otherwise
--   provides mouse press information.
type MouseEvent = Maybe (Bool, Int)

-- | A class for things which change within an interactive context.  The default
--   method implementations do nothing.
--
--   The first type parameter specifies which backend this interactive instance
--   is intended for.
class Interactive b a where
  -- | @tick@ is (ideally) called every 30ms.  The 'Bool' result indicates if
  --   the graphics need to be refreshed.
  tick                     :: InputState b -> a -> IO (a, Bool)

  -- | @mouse@ is called when the mouse moves or presses occur.
  mouse    :: MouseEvent   -> InputState b -> a -> IO a

  -- | @keyboard@ is called on key-presses.  
  keyboard :: KeyEvent     -> InputState b -> a -> IO a

  -- No-op defaults.
  tick _ = return . (, False)
  mouse    _ _ = return
  keyboard _ _ = return

-- InputState Queries.

-- | Gets the information for the most recent key event of the named key.
keyInfo :: String -> InputState b -> Maybe (KeyInfo b)
keyInfo name = M.lookup name . keyTable

-- | Gets whether the named key is held down.
keyHeld :: String -> InputState b -> Bool
keyHeld name (keyInfo name -> Just (True, _, _)) = True
keyHeld _ _ = False

-- | Whether the indicated mouse button is considered pressed in the InputState.
mouseHeld :: Int -> InputState b -> Bool
mouseHeld ix = keyHeld ("Mouse" ++ show ix)

-- | Converts a pure state transform to a function for Interactive 'tick'.
simpleTick :: (a -> a)
           -> InputState b -> a -> IO (a, Bool)
simpleTick f _ = return . (, True) . f

-- | Converts a function which responds to mouse-presses, and transforms state
--   accordingly to a function for Interactive 'mouse'.
simpleMouse :: (MouseEvent -> MousePos b -> a -> a)
            -> (MouseEvent -> InputState b -> a -> IO a)
simpleMouse f c inp = return . f c (mousePos inp)

-- | Converts a function which responds to mouse-presses, and transforms state
--   accordingly to a function for Interactive 'mouse'.
simpleMouseClick :: ((Bool, Int) -> MousePos b -> a -> a)
                 -> (MouseEvent -> InputState b -> a -> IO a)
simpleMouseClick f (Just c) inp = return . f c (mousePos inp)
simpleMouseClick _ _ _ = return

simpleMousePos :: (MousePos b -> a -> a)
               -> (MouseEvent -> InputState b -> a -> IO a)
simpleMousePos f _ inp = return . f (mousePos inp)

-- | Converts a function which responds to key-presses, and transforms state
--   accordingly to a function for Interactive 'keyboard'.
simpleKeyboard :: (KeyEvent -> a -> a)
               -> (KeyEvent -> InputState b -> a -> IO a)
simpleKeyboard f e _ = return . f e

-- | KeyHandlers allow you to monadically sequence a series of different
--   functions to handle keys, without worrying about plumbing the parameters.
type KeyHandler b a = StateT (KeyEvent, InputState b, a) IO ()

-- | Convenience function for turning a 'KeyHandler's into a function
--   appropriate for 'keyboard'.
handleKeys :: KeyHandler b a -> KeyEvent -> InputState b -> a -> IO a
handleKeys kh ke is x = liftM (\(_,_,x) -> x) $ execStateT kh (ke, is, x)

-- | Turns a function from 'KeyEvent's to imperative state mutation into a
--   keyHandler.  It's handy to use this with 'Control.Monad.when'.
keyHandler :: (KeyEvent -> a -> IO a) -> KeyHandler b a
keyHandler f = do
  (ke, is, x) <- get
  x' <- lift $ f ke x
  put (ke, is, x')