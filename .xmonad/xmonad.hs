import qualified Data.Map                            as M
import           Data.Monoid
import           System.Exit
import           System.IO
import           XMonad
import           XMonad.Actions.CopyWindow
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Grid
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Spiral
import           XMonad.Layout.Tabbed
import qualified XMonad.StackSet                     as W
import           XMonad.Util.EZConfig                (additionalKeys)
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run                     (spawnPipe)

main :: IO ()
main = do
    din <- spawnPipe "xmobar"
    xmonad $ docks $ defaults din

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP xmobarPP
    { ppHidden  = xmobarColor "#6c71c4" ""
    , ppCurrent = xmobarColor "#b58900" "" . wrap "[" "]"
    , ppTitle   = xmobarColor "#268bd2" "" . shorten 70
    , ppVisible = xmobarColor "#839496" "" . wrap "(" ")"
    , ppUrgent  = xmobarColor "#dc322f" "" . wrap " " " "
    , ppLayout  = xmobarColor "#2aa198" ""
    , ppOutput  = hPutStrLn h
    }

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
    [ className =? "chromium"          --> doShift "4"
    , className =? "Firefox"           --> doShift "5"
    , resource  =? "desktop_window"    --> doIgnore
    , className =? "Galculator"        --> doFloat
    , className =? "Gimp"              --> doFloat
    , className =? "Gitk"              --> doCenterFloat
    , className =? "XCalc"             --> doFloat
    , className =? "MPlayer"           --> doFloat
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)]

myLayouts = smartBorders $ mkToggle (NOBORDERS ?? FULL ?? EOT) standardLayout
    where
        standardLayout = avoidStruts ( tall ||| Full ||| Grid ||| Mirror tall )
            where
                tall = Tall nmaster delta ratio
                nmaster = 1
                ratio = 1/2
                delta = 2/100

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
    , ((modMask, xK_b), sendMessage $ Toggle FULL)
    , ((modMask, xK_h), sendMessage Shrink)
    , ((modMask, xK_l), sendMessage Expand)
    , ((modMask, xK_t), withFocused $ windows . W.sink)
    , ((modMask .|. shiftMask, xK_t), rectFloatFocused)
    , ((modMask, xK_g), namedScratchpadAction myScratchPads "todo")
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
        | (key, sc) <- zip [xK_u, xK_o, xK_p] [0..]
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

myScratchPads = [NS "todo" spawnTerm findTerm manageTerm]
                    where
                        spawnTerm = "urxvtc -name todo-scr -e vim ~/todo.md"
                        findTerm = resource =? "todo-scr"
                        manageTerm = doRectFloat $ W.RationalRect 0.2 0.2 0.6 0.6
