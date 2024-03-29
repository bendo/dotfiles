import qualified Data.Map                            as M
import           Data.Monoid
import           System.Exit
import           System.IO
import           XMonad

import           XMonad.Actions.CopyWindow

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops           (ewmh, fullscreenEventHook)
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName

import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Grid
import           XMonad.Layout.Magnifier
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Spiral
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns

import qualified XMonad.StackSet                     as W

import           XMonad.Util.EZConfig                (additionalKeys)
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run                     (spawnPipe)

main :: IO ()
main = do
    din <- spawnPipe "xmobar"
    xmonad $ ewmh $ docks $ defaults din

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP xmobarPP
    { ppSep     = magenta "  •  "
    , ppHidden  = xmobarColor "#6c71c4" ""
    , ppCurrent = wrap "" "" . xmobarBorder "Top" "#8be9fd" 2
    , ppTitle   = xmobarColor "#268bd2" "" . shorten 70
    , ppVisible = xmobarColor "#839496" "" . wrap "(" ")"
    , ppUrgent  = xmobarColor "#dc322f" "" . wrap " " " "
    , ppLayout  = xmobarColor "#2aa198" ""
    , ppOutput  = hPutStrLn h
    }

magenta :: String -> String
magenta  = xmobarColor "#ff79c6" ""

myStartupHook :: X ()
myStartupHook = spawn "xmobar ~/.xmobarrc"

defaults din = def
    { terminal           = "urxvtc"
    , focusFollowsMouse  = False
    , clickJustFocuses   = False
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

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1..9] ++ ["λ","π","ω"]

myManageHook :: ManageHook
myManageHook = namedScratchpadManageHook myScratchPads <+> composeAll
    [ className =? "chromium"           --> doShift "4"
    , className =? "Sabaki"             --> doFloat
    , className =? "pentablet"          --> doFloat
    , resource  =? "desktop_window"     --> doIgnore
    , className =? "Galculator"         --> doFloat
    , className =? "zoom"               --> doFloat
    , className =? "Gimp"               --> doFloat
    , className =? "Gitk"               --> doCenterFloat
    , className =? "XCalc"              --> doFloat
    , className =? "MPlayer"            --> doFloat
    , className =? "minecraft-launcher" --> doFloat
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)]

myLayouts = smartBorders $ mkToggle (NOBORDERS ?? FULL ?? EOT) standardLayout
    where
        standardLayout = avoidStruts ( tall ||| Full ||| threeCol ||| Grid )
            where
                threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta3 ratio3
                tall = Tall nmaster delta ratio
                nmaster = 1
                ratio = 1/2
                ratio3 = 1/2.5
                delta = 2/100
                delta3 = 3/100

black = "#020202"
gray  = "#7c7c7c"
green = "#5bce2d"
white = "#eeeeee"
blue  = "#268bd2"

tabConfig :: Theme
tabConfig = def
    { activeBorderColor   = gray
    , activeTextColor     = blue
    , activeColor         = black
    , inactiveBorderColor = gray
    , inactiveTextColor   = white
    , inactiveColor       = black
    }

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig {XMonad.modMask = modMask} = M.fromList $
    [ ((modMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask, xK_x), spawn "xkill")
    , ((modMask .|. shiftMask, xK_l), spawn "lock")
    , ((modMask, xK_d), spawn "dmenu_run -fn '-12'")
    , ((modMask, xK_q), kill)
    , ((modMask, xK_space), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
    , ((modMask, xK_n), refresh)
    , ((modMask, xK_Tab), windows W.focusDown)
    , ((modMask, xK_j), windows W.focusDown)
    , ((modMask, xK_k), windows W.focusUp)
    , ((modMask, xK_m), windows W.focusMaster)
    , ((modMask, xK_c), windows copyToAll)
    , ((modMask .|. shiftMask, xK_c), killAllOtherCopies)
    , ((modMask .|. shiftMask, xK_m), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j), windows W.swapDown)
    , ((modMask .|. shiftMask, xK_k), windows W.swapUp)
    , ((modMask, xK_z), sendMessage $ XMonad.Layout.MultiToggle.Toggle FULL)
    , ((modMask, xK_h), sendMessage Shrink)
    , ((modMask, xK_l), sendMessage Expand)
    , ((modMask, xK_t), withFocused $ windows . W.sink)
    , ((modMask .|. shiftMask, xK_t), rectFloatFocused)
    , ((modMask, xK_o), namedScratchpadAction myScratchPads "term")
    , ((modMask, xK_f), namedScratchpadAction myScratchPads "firefox")
    , ((modMask, xK_a), namedScratchpadAction myScratchPads "ghci")
    , ((modMask .|. shiftMask, xK_f), fullFloatFocused)
    , ((modMask, xK_comma), sendMessage (IncMasterN 1))
    , ((modMask, xK_period), sendMessage (IncMasterN (-1)))
    , ((modMask .|. shiftMask, xK_q), io exitSuccess)
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
        | (key, sc) <- zip [xK_u, xK_w, xK_e] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ] where
        fullFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f
        rectFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery doMyRectFloat f
            where
              doMyRectFloat = doRectFloat $ W.RationalRect 0.05 0.05 0.9 0.9

myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modMask} = M.fromList
    [ ((modMask, button1), \w -> focus w >> mouseMoveWindow w)
    , ((modMask, button2), \w -> focus w >> windows W.swapMaster)
    , ((modMask, button3), \w -> focus w >> mouseResizeWindow w)
    ]

centeredRect   = W.RationalRect 0.2 0.2 0.6 0.6
rightBarRect   = W.RationalRect (1/2) 0.019 (1/2) 0.98
leftBarRect    = W.RationalRect 0.001 0.019 (1/2) 0.98

myScratchPads = [ NS "term" spawnTerm findTerm manageTerm
                , NS "firefox" spawnFirefox findFirefox manageFirefox
                , NS "ghci" spawnGhci findGhci manageGhci
                ]
                    where
                        spawnTerm = "urxvtc -name term-scr"
                        findTerm = resource =? "term-scr"
                        manageTerm = doRectFloat $ W.RationalRect 0.2 0.2 0.6 0.6

                        spawnFirefox = "firefox"
                        findFirefox = className =? "firefox"
                        manageFirefox = doRectFloat $ rightBarRect

                        spawnGhci = "urxvtc -name ghci-scr -e stack ghci"
                        findGhci = resource =? "ghci-scr"
                        manageGhci = doRectFloat $ W.RationalRect 0.2 0.2 0.6 0.6
