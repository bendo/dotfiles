import System.IO
import System.Exit
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import qualified Data.Map as M

main = do
    din <- spawnPipe "xmobar"
    xmonad $ docks $ defaults din

myLogHook h = dynamicLogWithPP xmobarPP
    { ppHidden  = xmobarColor "#6c71c4" ""
    , ppCurrent = xmobarColor "#b58900" "" . wrap "[" "]"
    , ppTitle   = xmobarColor "#268bd2" "" . shorten 50
    , ppVisible = xmobarColor "#839496" "" . wrap "(" ")"
    , ppUrgent  = xmobarColor "#dc322f" "" . wrap " " " "
    , ppLayout  = xmobarColor "#2aa198" ""
    , ppOutput  = hPutStrLn h
    }

myStartupHook = do
    spawn "xmobar ~/.xmobarrc"

defaults din = def
    { terminal           = "urxvtc"
    , focusFollowsMouse  = False
    , borderWidth        = 1
    , normalBorderColor  = gray
    , focusedBorderColor = blue
    , modMask            = mod4Mask
    , logHook            = myLogHook din
    , workspaces         = myWorkspaces
    , keys               = myKeys
    , mouseBindings      = myMouseBindings
    , layoutHook         = myLayouts
    , manageHook         = myManageHook
    , startupHook        = myStartupHook
    }

{-
leafWS = "1:<fn=1>\xf06c</fn>"
dropWS = "2:<fn=1>\xf043</fn>"
codeWS = "3:<fn=1>\xf1b2</fn>"
webWS  = "4:<fn=1>\xf268</fn>"
-}

myWorkspaces = {-[leafWS, dropWS, codeWS, webWS]-} ["1","2","3","web"] ++ map show [5..9] ++ ["λ","π","ω"]

myManageHook = composeAll
    [ className =? "Chromium"          --> doShift "4"
    , resource  =? "desktop_window"    --> doIgnore
    , className =? "Galculator"        --> doFloat
    , className =? "Gimp"              --> doFloat
    , className =? "XCalc"             --> doFloat
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)]

myLayouts = onWorkspace "web" webLayout $ standardLayout
    where
        standardLayout = avoidStruts ( Mirror tall ||| tall ||| Grid ||| Full )
            where
                tall = Tall nmaster delta ratio
                nmaster = 1
                ratio = 1/2
                delta = 2/100
        webLayout = avoidStruts ( Mirror tall ||| Full )
            where
                tall = Tall nmaster delta ratio
                nmaster = 1
                ratio = 3/4
                delta = 2/100

black = "#020202"
gray  = "#7c7c7c"
green = "#5bce2d"
white = "#eeeeee"
blue  = "#268bd2"

tabConfig = def
    { activeBorderColor   = gray
    , activeTextColor     = blue
    , activeColor         = black
    , inactiveBorderColor = gray
    , inactiveTextColor   = white
    , inactiveColor       = black
    }

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask, xK_x), spawn "xkill")
    , ((modMask .|. shiftMask, xK_l), spawn "lock")
    , ((modMask, xK_d), spawn "dmenu_run -fn '-9'")
    , ((modMask, xK_q), kill)
    , ((modMask, xK_space), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
    , ((modMask, xK_n), refresh)
    , ((modMask, xK_Tab), windows W.focusDown)
    , ((modMask, xK_j), windows W.focusDown)
    , ((modMask, xK_k), windows W.focusUp)
    , ((modMask, xK_m), windows W.focusMaster)
    --, ((modMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j), windows W.swapDown)
    , ((modMask .|. shiftMask, xK_k), windows W.swapUp)
    , ((modMask, xK_h), sendMessage Shrink)
    , ((modMask, xK_l), sendMessage Expand)
    , ((modMask, xK_t), withFocused $ windows . W.sink)
    , ((modMask, xK_comma), sendMessage (IncMasterN 1))
    , ((modMask, xK_period), sendMessage (IncMasterN (-1)))
    , ((modMask .|. shiftMask, xK_q), io (exitWith ExitSuccess))
    , ((modMask, xK_r), restart "xmonad" True)
    , ((0, xK_Print), spawn "scrot '%Y-%m-%d-%T_$wx$h.png' -e 'mv $f ~'")
    ]
    ++
    [ ((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0, xK_minus, xK_equal])
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]
    ++
    [ ((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_u, xK_o, xK_p] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    ]

