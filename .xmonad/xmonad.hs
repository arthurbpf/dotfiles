import qualified Data.Map as M
import           Data.Monoid
import           Graphics.X11.ExtraTypes.XF86
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
import           XMonad.Layout.NoBorders (smartBorders)
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
myTerminal = "st"

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

    -- screen modes
    ((modm, xK_F1), spawn "~/.screenlayout/default.sh"),
    ((modm, xK_F2), spawn "~/.screenlayout/tv.sh"),
    ((modm, xK_F3), spawn "~/.screenlayout/all.sh"),
    -- launch steam
    ((modm, xK_s), spawn "steam"),
    -- launch lutris
    ((modm, xK_g), spawn "lutris"),
    -- launch internet browser
    ((modm, xK_i), spawn "chromium"),
    -- launch menu
    -- ((modm, xK_p), spawn "dmenu_run"),
    ((modm, xK_p), spawn "rofi -show combi -combi-modi \"drun,run,window,ssh\" -theme Arc-Dark"),
    -- launch emoji selector
    ((modm .|. shiftMask, xK_p), spawn "rofi -modi emoji -show emoji -kb-custom-1 Ctrl+C -theme Arc-Dark"),
    -- launch file manager
    ((modm, xK_slash), spawn "pcmanfm"),
    -- take a screenshot
    ((0, xK_Print), spawn "flameshot gui"),
    -- open copyq
    ((modm, xK_v), spawn "copyq show"),
    -- open DevDocs
    ((modm, xK_d), spawn "devdocs-desktop"),
    -- lock screen
    ((modm .|. shiftMask, xK_l), spawn "sleep 1 && xset dpms force suspend"),


    -- power button
    ((0, xF86XK_PowerDown), spawn "doas systemctl hibernate"),
    -- volume up
    ((0, xF86XK_AudioRaiseVolume),  spawn "amixer -D pulse sset Master 10%+"),
    -- volume down
    ((0, xF86XK_AudioLowerVolume),  spawn "amixer -D pulse sset Master 10%-"),
    -- volume mute
    ((0, xF86XK_AudioMute),         spawn "amixer -D pulse sset Master toggle"),
    -- brightness up
    ((0, xF86XK_MonBrightnessUp),   spawn "brightnessctl set +10%"),
    -- brightness down
    ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl set 10%-"),
    -- calc
    ((0, xF86XK_Calculator), spawn "qalculate-gtk")
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
  spawnOnce "udiskie &"
  spawnOnce "picom --experimental-backends &"
  spawnOnce "nitrogen --restore &"
  spawnOnce "xsetroot -cursor_name left_ptr &"
  spawnOnce "trayer --edge top --height 22 --width 10 --align right --transparent true --alpha 0 --tint 0x000000 &"
  spawnOnce "blueman-applet &"
  spawnOnce "pasystray &"
  spawnOnce "nm-applet &"
  spawnOnce "flameshot &"
  spawnOnce "dunst &"
  spawnOnce "copyq &"
  spawnOnce "lxqt-policykit-agent &"
  spawnOnce "xss-lock ~/.lock.sh &"
  spawnOnce "openrgb -p active &"
  spawnOnce "kdeconnect-indicator &"
  spawnOnce "redshift-gtk &"
  spawnOnce "powerkit &"
  return()

-- Run xmonad with the settings you specify. No need to modify this.
main = do
    xmproc <- spawnPipe "xmobar -x 0 $HOME/.xmobarrc"
    xmonad $ ewmhFullscreen $ ewmh $ docks $ def
        { terminal = myTerminal
        , focusFollowsMouse = myFocusFollowsMouse
        , clickJustFocuses = myClickJustFocuses
        , borderWidth = myBorderWidth
        , modMask = myModMask
        , workspaces = myWorkspaces
        , focusedBorderColor = color1
        , normalBorderColor = color2
        , keys = myKeys
        , layoutHook = smartBorders $ myLayout
        , manageHook = myManageHook
        , startupHook = myStartupHook
        , logHook = myLogHook xmproc
        , handleEventHook = handleEventHook def
        }
