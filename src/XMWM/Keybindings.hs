-- | Custom keybindings.
module XMWM.Keybindings (keybindings, superMask) where

import Relude

import Control.Monad.Except (MonadError, liftEither)
import Data.Bits ((.|.))
import Data.Default (def)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Graphics.X11.ExtraTypes.XF86 (
  xF86XK_AudioLowerVolume,
  xF86XK_AudioMedia,
  xF86XK_AudioMicMute,
  xF86XK_AudioMute,
  xF86XK_AudioNext,
  xF86XK_AudioPlay,
  xF86XK_AudioPrev,
  xF86XK_AudioRaiseVolume,
  xF86XK_MonBrightnessDown,
  xF86XK_MonBrightnessUp,
 )
import Graphics.X11.Types (KeyMask, KeySym, Window)
import Graphics.X11.Xlib (
  mod1Mask,
  mod3Mask,
  mod4Mask,
  noModMask,
  shiftMask,
  xK_Delete,
  xK_Down,
  xK_Escape,
  xK_F1,
  xK_F10,
  xK_F11,
  xK_F12,
  xK_F9,
  xK_Left,
  xK_Print,
  xK_Right,
  xK_Up,
  xK_b,
  xK_backslash,
  xK_c,
  xK_e,
  xK_l,
  xK_p,
  xK_q,
  xK_r,
  xK_t,
  xK_u,
  xK_v,
  xK_w,
 )
import XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace, addWorkspace, removeEmptyWorkspace)
import XMonad.Actions.PhysicalScreens (sendToScreen, viewScreen)
import XMonad.Core (ExtensionClass (..), ScreenId, X, spawn, withWindowSet)
import XMonad.Hooks.ManageDocks (ToggleStruts (..))
import XMonad.Operations (float, kill, sendMessage, sendRestart, windows, withFocused)
import XMonad.StackSet (
  Screen,
  Stack,
  current,
  delete',
  differentiate,
  down,
  greedyView,
  integrate',
  screen,
  screens,
  shift,
  sink,
  stack,
  visible,
  workspace,
 )
import XMonad.Util.ExtensibleState qualified as XS

import XMWM.Debug (debug)
import XMWM.Prompt (dmenu')
import XMWM.Sound.MPRIS.DBus (MPRIST, runMPRIST)
import XMWM.Sound.MPRIS.DBus.Player (Player (..), listPlayers)
import XMWM.Sound.MPRIS.DBus.Player qualified as Player (next, pause, play, playPause, previous)
import XMWM.Sound.Pulse.DBus (PulseAudioT)
import XMWM.Sound.Pulse.DBus.Card (
  CardProfile (..),
  getDefaultSinkCardProfiles,
  getDefaultSourceCardProfiles,
  setDefaultSinkCardProfile,
  setDefaultSourceCardProfile,
 )
import XMWM.Sound.Pulse.DBus.Device (
  deviceID,
  devicePrettyName,
  getSinks,
  getSources,
  setDefaultSink,
  setDefaultSource,
 )
import XMWM.Sound.Pulse.DBus.Server (runPulseAudioTSession)
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

-- | 'hotkey'', but shift must also be held.
hotkeyS' :: KeySym -> X () -> [Keybinding]
hotkeyS' = bindKey [shiftMask]

-- | 'hotkey', but only when the hotkey is pressed with one of the
-- 'defaultMasks'.
withMask :: KeySym -> String -> [Keybinding]
withMask = spawning withMask'

-- | 'hotkey'', but only when the hotkey is pressed with one of the
-- 'defaultMasks'.
withMask' :: KeySym -> X () -> [Keybinding]
withMask' = bindKey defaultMasks

-- | 'withMask', but shift must also be held.
withSMask :: KeySym -> String -> [Keybinding]
withSMask = spawning withSMask'

-- | 'withMask'', but shift must also be held.
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
    , mprisBindings
    , volumeBindings
    , audioDeviceBindings
    ]

newtype PinsState = PinsState (Set Window)

instance ExtensionClass PinsState where
  initialValue = PinsState def

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
      withMask' xK_r $ liftIO sendRestart
    , -- Exit XMonad
      withSMask' xK_r exitSuccess
    , -- Close focused window
      withMask' xK_c kill
    , -- Toggle struts (xmobar visibility)
      withMask' xK_b $ sendMessage ToggleStruts
    , -- Pin a window
      withSMask' xK_p $ withFocused $ \w ->
        XS.modifyM $ \(PinsState pins) ->
          PinsState
            <$> if Set.member w pins
              then Set.delete w pins <$ windows (sink w)
              else Set.insert w pins <$ float w
    , -- Unfloat a window. Unfloating pinned windows also unpins them.
      withMask' xK_t $ withFocused $ \w ->
        XS.modifyM $ \(PinsState pins) ->
          PinsState
            <$> (Set.delete w pins <$ windows (sink w))
    ]
    ++
    -- Swap workspace to monitor
    [ ((mask .|. extraMask, key), action workspaceId)
    | (key, workspaceId) <- defaultWorkspaces
    , (action, extraMask) <- [(viewWorkspace, noModMask), (windows . shift, shiftMask)]
    , mask <- defaultMasks
    ]
    ++
    -- Focus on physical monitor (triple-headed setup)
    [ ((mask .|. extraMask, key), action def screen)
    | (key, screen) <- zip [xK_q, xK_w, xK_e] [0 ..]
    , (action, extraMask) <- [(viewScreen, noModMask), (sendToScreen, shiftMask)]
    , mask <- defaultMasks
    ]
  where
    -- Like 'greedyView', but handles pinned windows.
    --
    -- From 'greedyView':
    --
    -- > Set focus to the given workspace. If that workspace does not exist in
    -- > the stackset, the original workspace is returned. If that workspace is
    -- > hidden, then display that workspace on the current screen, and move the
    -- > current workspace to hidden. If that workspace is visible on another
    -- > screen, the workspaces of the current screen and the other screen are
    -- > swapped.
    --
    -- This function first checks for pinned windows in the current workspace
    -- and notes their screen ID. It then performs a 'greedyView', and checks
    -- whether the screen ID of the pinned windows has changed. If so, it
    -- restores the pinned windows to their original pinned screen.
    viewWorkspace swapToWorkspaceId = withWindowSet $ \ws -> do
      -- In all visible screens, get the pinned windows.
      (PinsState pinnedSet) <- XS.get
      let
        visiblePinned = flip map (screens ws) $ \screen ->
          (screen.screen, filter (`Set.member` pinnedSet) $ integrate' screen.workspace.stack)
        pinnedWindows = concatMap snd visiblePinned
        pinMap = Map.fromList visiblePinned

      -- Remove the pinned windows from all visible screens.
      let pruned = foldl' (flip delete') ws pinnedWindows

      -- Swap workspaces.
      let swapped = greedyView swapToWorkspaceId pruned

      -- In all visible screens, restore pinned windows.
      let repin' = repin pinMap
      let repinned = swapped{current = repin' swapped.current, visible = map repin' swapped.visible}
      windows $ const repinned
      where
        repin :: Map ScreenId [Window] -> Screen i l Window ScreenId sd -> Screen i l Window ScreenId sd
        repin pinMap screen =
          screen{workspace = screen.workspace{stack = insertBottom screen.workspace.stack $ windowsForScreen screen.screen}}
          where
            windowsForScreen :: ScreenId -> [Window]
            windowsForScreen screenId = fromMaybe [] $ Map.lookup screenId pinMap

        insertBottom :: Maybe (Stack a) -> [a] -> Maybe (Stack a)
        insertBottom Nothing xs = differentiate xs
        insertBottom (Just s) xs = Just $ s{down = s.down ++ xs}

utilBindings :: [Keybinding]
utilBindings =
  concat
    [ -- Launcher (see `~/.profile` re: $PATH)
      -- TODO: Ship a portable version of yeganesh that builds with XMWM.
      withMask xK_p "$(yegonesh -x)"
    , -- Lock screen
      withSMask xK_l "slock"
    , withSMask xK_u "slock dm-tool lock"
    , -- Forcibly xkill a clicked window
      withSMask xK_c "xkill"
    , -- Screenshots
      hotkey xK_Print "maim ~/screenshots/$(date +%s).png"
    , withMask xK_Print "maim --select --hidecursor ~/screenshots/$(date +%s).png"
    , withSMask xK_Print "maim --select --hidecursor --tolerance 9999999 ~/screenshots/$(date +%s).png"
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
      hotkey xF86XK_AudioMute volumeToggleMuteSh
    , hotkey xF86XK_AudioLowerVolume volumeLowerSh
    , hotkey xF86XK_AudioRaiseVolume volumeRaiseSh
    , -- Microphone control with media keys
      hotkey xF86XK_AudioMicMute "amixer sset Capture toggle"
    ]

newtype MPRISState = MPRISState (Maybe Player)

instance ExtensionClass MPRISState where
  initialValue = MPRISState Nothing

mprisBindings :: [Keybinding]
mprisBindings =
  concat
    [ -- Select MPRIS players
      hotkeyS' xF86XK_AudioPlay selectPlayer
    , withSMask' xK_Up selectPlayer
    , hotkey' xF86XK_AudioMedia selectPlayer
    , -- Control MPRIS players
      hotkey' xF86XK_AudioPlay $ player Player.playPause
    , withMask' xK_Up $ player Player.play
    , withMask' xK_Down $ player Player.pause
    , hotkey' xF86XK_AudioNext $ player Player.next
    , withMask' xK_Right $ player Player.next
    , hotkey' xF86XK_AudioPrev $ player Player.previous
    , withMask' xK_Left $ player Player.previous
    ]
  where
    run action = do
      result <- runExceptT action
      case result of
        Left err -> debug $ "Could not run MPRIS action: " <> err
        Right _ -> pass
    runMPRIST' action = liftEither =<< liftIO (runMPRIST action)

    selectPlayer :: X ()
    selectPlayer = run $ do
      players <- runMPRIST' listPlayers
      selected <- dmenu' (.identity) players
      lift $ XS.put $ MPRISState $ Just selected

    player :: (Player -> MPRIST IO ()) -> X ()
    player action = run $ do
      selected <- do
        (MPRISState selectedPlayer) <- lift XS.get
        liftEither $ maybeToRight "No player selected" selectedPlayer
      runMPRIST' $ action selected

volumeBindings :: [Keybinding]
volumeBindings =
  concat
    [ -- Toggle mute
      withMask xK_F9 volumeToggleMuteSh
    , withMask xK_F10 "amixer sset Capture toggle"
    , -- Volume control
      withMask xK_F11 volumeLowerSh
    , withMask xK_F12 volumeRaiseSh
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
        Left err -> debug $ "Could not select PulseAudio device: " <> err

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

selectOption ::
  (MonadIO m, MonadError String m, ToString s) =>
  m [a] ->
  (a -> s) ->
  (a -> m b) ->
  m b
selectOption options render setOption = do
  opts <- options
  selected <- dmenu' (toString . render) opts
  setOption selected

-- TODO: Auto-configure Pipewire vs PulseAudio.
volumeToggleMuteSh :: String
-- volumeToggleMuteSh = "amixer sset Master toggle"
volumeToggleMuteSh = "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"

volumeLowerSh :: String
-- volumeLowerSh = "amixer sset Master 1%-"
volumeLowerSh = "wpctl set-volume @DEFAULT_AUDIO_SINK@ 1%-"

volumeRaiseSh :: String
-- volumeRaiseSh = "amixer sset Master 1%+"
volumeRaiseSh = "wpctl set-volume --limit 1 @DEFAULT_AUDIO_SINK@ 1%+"
