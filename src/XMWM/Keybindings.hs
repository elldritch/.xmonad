-- | Custom keybindings.
module XMWM.Keybindings (keybindings, superMask) where

import Relude

import Data.Bits ((.|.))
import Data.Default (def)
import Graphics.X11.ExtraTypes.XF86 (
  xF86XK_AudioLowerVolume,
  xF86XK_AudioMicMute,
  xF86XK_AudioMute,
  xF86XK_AudioRaiseVolume,
  xF86XK_MonBrightnessDown,
  xF86XK_MonBrightnessUp,
 )
import Graphics.X11.Types (KeyMask, KeySym)
import Graphics.X11.Xlib (
  mod1Mask,
  mod3Mask,
  mod4Mask,
  noModMask,
  shiftMask,
  xK_Delete,
  xK_Escape,
  xK_F1,
  xK_F10,
  xK_F11,
  xK_F12,
  xK_F9,
  xK_Print,
  xK_b,
  xK_backslash,
  xK_c,
  xK_e,
  xK_l,
  xK_p,
  xK_q,
  xK_r,
  xK_u,
  xK_v,
  xK_w,
 )
import XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace, addWorkspace, removeEmptyWorkspace)
import XMonad.Actions.PhysicalScreens (sendToScreen, viewScreen)
import XMonad.Core (X, getDirectories, recompile, spawn)
import XMonad.Hooks.ManageDocks (ToggleStruts (..))
import XMonad.Operations (kill, restart, sendMessage, windows)
import XMonad.StackSet (greedyView, shift)

import Sound.Pulse.DBus (PulseAudioT)
import Sound.Pulse.DBus.Card (
  CardProfile (..),
  getDefaultSinkCardProfiles,
  getDefaultSourceCardProfiles,
  setDefaultSinkCardProfile,
  setDefaultSourceCardProfile,
 )
import Sound.Pulse.DBus.Device (
  deviceID,
  devicePrettyName,
  getSinks,
  getSources,
  setDefaultSink,
  setDefaultSource,
 )
import Sound.Pulse.DBus.Server (runPulseAudioTSession)
import XMWM.Debug (debug)
import XMWM.Prompt (dmenu')
import XMWM.Workspaces (defaultWorkspaces, workspaceFromDmenu)

-- Masks

-- | The \"super\" (aka \"Start\") key.
superMask :: KeyMask
superMask = mod4Mask

-- | The left alt key.
_leftAltMask :: KeyMask
_leftAltMask = mod1Mask

-- | The right alt key. For this to work, you need a custom @xmodmap@. See also:
--
-- - https://unix.stackexchange.com/questions/238877/can-xmonad-treat-left-and-right-alt-differently
-- - https://unix.stackexchange.com/questions/239988/why-isnt-x-treating-alt-l-and-alt-r-differently-w-r-t-mod1
rightAltMask :: KeyMask
rightAltMask = mod3Mask

-- | By default, use both super and right-alt as mask keys.
defaultMasks :: [KeyMask]
defaultMasks = [superMask, rightAltMask]

withoutMasks :: [KeyMask]
withoutMasks = [noModMask]

-- Keybinding utilities

type Keybinding = ((KeyMask, KeySym), X ())

-- | When this key is pressed, run a shell command.
hotkey :: KeySym -> String -> [Keybinding]
hotkey = spawning hotkey'

-- | When this key is pressed, run an action.
hotkey' :: KeySym -> X () -> [Keybinding]
hotkey' = bindKey withoutMasks

-- | `hotkey`, but only when the hotkey is pressed with one of the
-- `defaultMasks`.
withMask :: KeySym -> String -> [Keybinding]
withMask = spawning withMask'

-- | `hotkey'`, but only when the hotkey is pressed with one of the
-- `defaultMasks`.
withMask' :: KeySym -> X () -> [Keybinding]
withMask' = bindKey defaultMasks

-- | `withMask`, but shift must also be held.
withSMask :: KeySym -> String -> [Keybinding]
withSMask = spawning withSMask'

-- | `withMask'`, but shift must also be held.
withSMask' :: KeySym -> X () -> [Keybinding]
withSMask' = bindKey $ (.|. shiftMask) <$> defaultMasks

spawning :: (KeySym -> X () -> [Keybinding]) -> KeySym -> String -> [Keybinding]
spawning makeKeybinding key = makeKeybinding key . spawn

bindKey :: [KeyMask] -> KeySym -> X () -> [Keybinding]
bindKey masks key action = [((mask, key), action) | mask <- masks]

-- Keybindings

keybindings :: [Keybinding]
keybindings =
  concat
    [ coreBindings
    , utilBindings
    , mediaKeyBindings
    , volumeBindings
    , audioDeviceBindings
    ]

coreBindings :: [Keybinding]
coreBindings =
  concat
    [ -- Dynamic workspaces
      withMask' xK_backslash $
        workspaceFromDmenu >>= maybe pass addWorkspace
    , withSMask' xK_backslash $ do
        selected <- workspaceFromDmenu
        case selected of
          Just ws -> do
            addHiddenWorkspace ws
            windows $ shift ws
          Nothing -> pass
    , withMask' xK_Delete removeEmptyWorkspace
    , -- Recompile and reload XMonad
      withMask' xK_r $ do
        dirs <- liftIO getDirectories
        ok <- recompile dirs False
        if ok then restart "xmonad" True else spawn "xmessage recompile failed"
    , -- Exit XMonad
      withSMask' xK_r exitSuccess
    , -- Close focused window
      withMask' xK_c kill
    , -- Toggle struts (xmobar visibility)
      withMask' xK_b $ sendMessage ToggleStruts
    ]
    ++
    -- Swap workspace to monitor
    [ ((mask .|. extraMask, key), windows $ action workspace)
    | (key, workspace) <- defaultWorkspaces
    , (action, extraMask) <- [(greedyView, noModMask), (shift, shiftMask)]
    , mask <- defaultMasks
    ]
    ++
    -- Focus on physical monitor (triple-headed setup)
    [ ((mask .|. extraMask, key), action def screen)
    | (key, screen) <- zip [xK_q, xK_w, xK_e] [0 ..]
    , (action, extraMask) <- [(viewScreen, noModMask), (sendToScreen, shiftMask)]
    , mask <- defaultMasks
    ]

utilBindings :: [Keybinding]
utilBindings =
  concat
    [ -- Launcher (see `~/.profile` re: $PATH)
      withMask xK_p "$(yeganesh -x)"
    , -- Lock screen
      withSMask xK_l "slock"
    , withSMask xK_u "slock dm-tool lock"
    , -- Forcibly xkill a clicked window
      withSMask xK_c "xkill"
    , -- Screenshots
      hotkey xK_Print "maim ~/screenshots/$(date +%s).png"
    , withMask xK_Print "maim --select --hidecursor ~/screenshots/$(date +%s).png"
    , -- Clipboard manager
      withMask xK_v "gpaste-client ui"
    , -- Notifications daemon
      withMask xK_Escape "dunstctl close-all"
    , withMask xK_F1 "dunstctl set-paused toggle"
    ]

mediaKeyBindings :: [Keybinding]
mediaKeyBindings =
  concat
    [ -- Screen brightness
      hotkey xF86XK_MonBrightnessUp "xbacklight -inc 10"
    , hotkey xF86XK_MonBrightnessDown "xbacklight -dec 10"
    , -- Volume control with media keys
      hotkey xF86XK_AudioMute "amixer sset Master toggle"
    , hotkey xF86XK_AudioLowerVolume "amixer sset Master 1%-"
    , hotkey xF86XK_AudioRaiseVolume "amixer sset Master 1%+"
    , -- Microphone control with media keys
      hotkey xF86XK_AudioMicMute "amixer sset Capture toggle"
    ]

volumeBindings :: [Keybinding]
volumeBindings =
  concat
    [ -- Toggle mute
      withMask xK_F9 "amixer sset Master toggle"
    , withMask xK_F10 "amixer sset Capture toggle"
    , -- Volume control
      withMask xK_F11 "amixer sset Master 1%-"
    , withMask xK_F12 "amixer sset Master 1%+"
    ]

audioDeviceBindings :: [Keybinding]
audioDeviceBindings =
  concat
    [ -- Select default sink
      withSMask' xK_F9 selectDefaultSink
    , -- Select default source
      withSMask' xK_F10 selectDefaultSource
    , -- Select profile for default sink card
      withSMask' xK_F11 selectDefaultSinkCardProfile
    , -- Select profile for default source card
      withSMask' xK_F12 selectDefaultSourceCardProfile
    ]
  where
    runPA :: (MonadIO m) => PulseAudioT IO () -> m ()
    runPA action = do
      result <- liftIO $ runPulseAudioTSession action
      case result of
        Right () -> pass
        Left err -> debug $ "Could not select default sink: " <> err

    selectOption ::
      (MonadIO m, ToString s) =>
      PulseAudioT m [a] ->
      (a -> s) ->
      (a -> PulseAudioT m ()) ->
      PulseAudioT m ()
    selectOption options render setOption = do
      opts <- options
      selected <- dmenu' (toString . render) opts
      maybe pass setOption selected

    selectDefaultSink :: (MonadIO m) => m ()
    selectDefaultSink =
      runPA $ selectOption getSinks devicePrettyName (setDefaultSink . deviceID)

    selectDefaultSource :: (MonadIO m) => m ()
    selectDefaultSource =
      runPA $ selectOption getSources devicePrettyName (setDefaultSource . deviceID)

    -- Why does this cause the default sink itself to change when the profile is
    -- set to something strange?
    --
    -- For example, when I set my Built-in Audio to use Digital Output, the
    -- default sink automatically switches? This appears to happen using
    -- pavucontrol too, and pactl reports that the default sink is actually
    -- changing (as opposed to the sink-inputs just moving or not being heard).
    --
    -- Okay, some testing with `pactl info` and `pavucontrol`:
    --
    -- - Nonsensical card profile changes cause the default sink to change
    -- - Nonsensical card profile changes do NOT cause streams to move - if you
    --   change the profile back, sound plays again even if the default sink is
    --   still the changed sink.
    selectDefaultSinkCardProfile :: (MonadIO m) => m ()
    selectDefaultSinkCardProfile =
      runPA $ selectOption getDefaultSinkCardProfiles description (setDefaultSinkCardProfile . profileID)

    selectDefaultSourceCardProfile :: (MonadIO m) => m ()
    selectDefaultSourceCardProfile =
      runPA $ selectOption getDefaultSourceCardProfiles description (setDefaultSourceCardProfile . profileID)
