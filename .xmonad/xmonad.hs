--
--  @arthurbpf's config file.
--

-- Imports

-- Data
import qualified Data.Map as M
import           Data.Monoid

-- System
import           System.Exit
import           System.IO

-- XMonad
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
color1 :: String
color1 = "#9942c3"

color2 :: String
color2 = "#dddddd"

color3 :: String
color3 = "#02efe7"

-- Borders width
myBorderWidth :: Dimension
myBorderWidth = 3

-- Default terminal emulator
myTerminal :: String
myTerminal = "alacritty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Set mod key
myModMask :: KeyMask
myModMask = mod4Mask -- sets to "super key"

-- Key bindings
myKeys conf@(XConfig {XMonad.modMask = modm}) =
  M.fromList $
    [ 
    -- launch a terminal
    ((modm, xK_Return), spawn $ XMonad.terminal conf),
    -- launch dmenu
    ((modm, xK_p), spawn "dmenu_run"),
    -- launch firefox
    ((modm, xK_f), spawn "firefox"),
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
    ((modm .|. shiftMask, xK_Return), windows W.swapMaster),
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
    ((modm, xK_q), spawn "xmonad --recompile; xmonad --restart")
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

-- Mouse bindings: default actions bound to mouse events
myMouseBindings (XConfig {XMonad.modMask = modm}) =
  M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ( (modm, button1),
        ( \w ->
            focus w >> mouseMoveWindow w
              >> windows W.shiftMaster
        )
      ),
      -- mod-button2, Raise the window to the top of the stack
      ((modm, button2), (\w -> focus w >> windows W.shiftMaster)),
      -- mod-button3, Set the window to floating mode and resize by dragging
      ( (modm, button3),
        ( \w ->
            focus w >> mouseResizeWindow w
              >> windows W.shiftMaster
        )
      )
      -- you may also bind events to the mouse scroll wheel (button4 and button5)

    ]

-- Layouts:
-- Layout params

-- Tall
-- 1: The default number of windows in the master pane
-- 2: Percent of screen to increment by when resizing panes
-- 3: Default proportion of screen occupied by master pane

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

tall = renamed [Replace "tall"] $ mySpacing 2 $ Tall 1 (3/100) (1/2)
fat = renamed [Replace "fat"] $ Mirror tall
full = renamed [Replace "full"] $ mySpacing 0 $ Full

myLayout = avoidStruts (tall ||| full ||| fat)


-- Set workspaces
windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x   = [x]

myWorkspaces = clickable . (map xmobarEscape)
               -- $ ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
               $ ["dev", "www", "sys", "doc", "vbox", "chat", "mus", "vid", "gfx"]
    where                                                                       
        clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                            (i,ws) <- zip [1..9] l,                                        
                            let n = i ]


-- Window rules:
myManageHook =
  composeAll
    [ className =? "MPlayer" --> doFloat
    , className =? "Gimp" --> doFloat
    , className =? "rdesktop" --> doFloat
    , resource =? "desktop_window" --> doIgnore
    , resource =? "kdesktop" --> doIgnore
    ]

-- Status bars and logging
myLogHook h = do 
  dynamicLogWithPP xmobarPP
    { ppOutput  = hPutStrLn h
    , ppTitle   = xmobarColor color1 "" . shorten 100  -- Title of active window
    , ppCurrent = xmobarColor color3 "" . wrap "[" "]" -- Current workspace in xmobar
    , ppVisible = xmobarColor color3 "" . wrap "(" ")" -- Visible but not current workspace 
    , ppHidden  = xmobarColor color2 "" . wrap "*" ""  -- Hidden workspaces in xmobar
    , ppUrgent  = xmobarColor "red"  "" . wrap "!" "!" -- Urgent workspace
    , ppHiddenNoWindows = xmobarColor "grey" ""        -- Hidden workspaces (no windows) 
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
  spawnOnce "$HOME/.g512.sh &"
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
        , mouseBindings = myMouseBindings
        , layoutHook = myLayout
        , manageHook = myManageHook
        , startupHook = myStartupHook
        , logHook = myLogHook xmproc
        , handleEventHook = handleEventHook def <+> fullscreenEventHook
        }