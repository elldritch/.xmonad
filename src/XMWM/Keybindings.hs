-- | Various custom keybindings.
module XMWM.Keybindings (superMask) where

import Graphics.X11.Types (KeyMask)
import Graphics.X11.Xlib (mod4Mask)

-- | The \"super\" (aka \"Start\") key.
superMask :: KeyMask
superMask = mod4Mask
