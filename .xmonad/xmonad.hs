import qualified Data.Map as M
import           Data.Monoid
import           System.Exit
import           System.IO
import           XMonad
import           XMonad.Config.Desktop
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.Gaps
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.Renamed (renamed, Rename(Replace))
import           XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig (additionalKeysP)
import           XMonad.Util.Run (spawnPipe)
import           XMonad.Util.SpawnOnce

-- Colors definition
color1 = "#ffffff"
color2 = "#555555"
color3 = "#aaaaaa"
color4 = "#02efe7"

-- Borders width
myBorderWidth :: Dimension
myBorderWidth = 1

-- Terminal emulator
myTerminal :: String
myTerminal = "kitty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Set mod key
myModMask :: KeyMask
myModMask = mod4Mask

-- Key bindings
myKeys conf@(XConfig {XMonad.modMask = modm}) =
  M.fromList $
    [
    -- launch a terminal
    ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf),
    -- close focused window
    ((modm, xK_c), kill),
    -- Rotate through the available layout algorithms
    ((modm, xK_space), sendMessage NextLayout),
    -- Reset the layouts on the current workspace to default
    ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf),
    -- Resize viewed windows to the correct size
    ((modm, xK_n), refresh),
    -- Move focus to the next window
    ((modm, xK_Tab), windows W.focusDown),
    -- Move focus to the next window
    ((modm, xK_j), windows W.focusDown),
    -- Move focus to the previous window
    ((modm, xK_k), windows W.focusUp),
    -- Move focus to the master window
    ((modm, xK_m), windows W.focusMaster),
    -- Swap the focused window and the master window
    ((modm, xK_Return), windows W.swapMaster),
    -- Swap the focused window with the next window
    ((modm .|. shiftMask, xK_j), windows W.swapDown),
    -- Swap the focused window with the previous window
    ((modm .|. shiftMask, xK_k), windows W.swapUp),
    -- Shrink the master area
    ((modm, xK_h), sendMessage Shrink),
    -- Expand the master area
    ((modm, xK_l), sendMessage Expand),
    -- Push window back into tiling
    ((modm, xK_t), withFocused $ windows . W.sink),
    -- Increment the number of windows in the master area
    ((modm, xK_comma), sendMessage (IncMasterN 1)),
    -- Deincrement the number of windows in the master area
    ((modm, xK_period), sendMessage (IncMasterN (-1))),
    -- Toggles status bar
    ((modm, xK_b), sendMessage ToggleStruts),
    -- Quit xmonad
    ((modm .|. shiftMask, xK_q), io (exitWith ExitSuccess)),
    -- Restart xmonad
    ((modm, xK_q), spawn "xmonad --recompile; xmonad --restart"),

    -- launch firefox
    ((modm, xK_f), spawn "firefox"),
    -- launch dmenu
    ((modm, xK_p), spawn "dmenu_run")
    ]
    ++
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [ ((m .|. modm, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9],
        (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]
    ++
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..],
        (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]

-- Layouts:
-- Layout params
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

tall = renamed [Replace "tall"] $ mySpacing 2 $ Tall 1 (3/100) (1/2)
fat = renamed [Replace "fat"] $ Mirror tall
full = renamed [Replace "full"] $ mySpacing 0 $ Full

myLayout = avoidStruts (tall ||| full ||| fat)

-- Set workspaces
windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- myWorkspaces = ["dev", "www", "sys", "doc", "chat", "media", "misc_0", "misc_1", "misc_2"]
myWorkspaces = ["I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"]

-- Window rules:
myManageHook =
  composeAll
    [ className =? "MPlayer" --> doFloat
    , className =? "Gimp" --> doFloat
    , className =? "rdesktop" --> doFloat
    ]

-- Status bars and logging
myLogHook h = do
  dynamicLogWithPP xmobarPP
    { ppOutput  = hPutStrLn h
    , ppTitle   = xmobarColor color1 "" . shorten 100  -- Title of active window
    , ppCurrent = xmobarColor color4 "" . wrap "[" "]" -- Current workspace in xmobar
    , ppVisible = xmobarColor color4 "" . wrap "(" ")" -- Visible but not current workspace
    , ppHidden  = xmobarColor color3 "" . wrap "*" ""  -- Hidden workspaces in xmobar
    , ppUrgent  = xmobarColor "red"  "" . wrap "!" "!" -- Urgent workspace
    , ppHiddenNoWindows = xmobarColor color2 ""        -- Hidden workspaces (no windows)
    , ppExtras  = [windowCount]                        -- # of windows current workspace
    , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
    }

-- Startup hook
myStartupHook = do
  spawnOnce "setxkbmap -layout us,us -variant ,intl -option 'grp:alt_space_toggle' &"
  spawnOnce "picom --experimental-backends &"
  spawnOnce "nitrogen --restore &"
  spawnOnce "dunst &"
  spawnOnce "copyq &"
  spawnOnce "udiskie &"
  spawnOnce "/usr/lib/kdeconnectd &"
  spawnOnce "xsetroot -cursor_name left_ptr &"
  spawnOnce "$HOME/.g512.sh &"
  spawnOnce "redshift -l -28.72708:-49.22799 &"
  return()

-- Run xmonad with the settings you specify. No need to modify this.
main = do
    xmproc <- spawnPipe "xmobar -x 0 $HOME/.xmobarrc"
    xmonad $ ewmh $ docks $ def
        { terminal = myTerminal
        , focusFollowsMouse = myFocusFollowsMouse
        , clickJustFocuses = myClickJustFocuses
        , borderWidth = myBorderWidth
        , modMask = myModMask
        , workspaces = myWorkspaces
        , focusedBorderColor = color1
        , normalBorderColor = color2
        , keys = myKeys
        , layoutHook = myLayout
        , manageHook = myManageHook
        , startupHook = myStartupHook
        , logHook = myLogHook xmproc
        , handleEventHook = handleEventHook def <+> fullscreenEventHook
        }
