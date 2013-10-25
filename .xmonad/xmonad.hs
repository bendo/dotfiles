import System.IO
import System.Exit
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import qualified Data.Map as M

main = do
	xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
	xmonad $ defaults
		{ logHook = dynamicLogWithPP $ xmobarPP
			{ ppOutput = hPutStrLn xmproc
			, ppTitle = xmobarColor colorBlue "" . shorten 100
			, ppCurrent = xmobarColor colorBlue ""
			, ppSep = " "
			}
		, manageHook = manageDocks <+> myManageHook
		, startupHook = setWMName "LG3D"
		}

defaults = defaultConfig
	{ terminal 		= "urxvtc"
	, focusFollowsMouse 	= True
	, borderWidth 		= 1
	, modMask 		= mod4Mask
	, workspaces 		= myWorkspaces
	, normalBorderColor 	= colorGray
	, focusedBorderColor 	= colorGreen
	, keys 			= myKeys
	, mouseBindings 	= myMouseBindings
	, layoutHook 		= smartBorders $ myLayout
	, manageHook 		= myManageHook
	, startupHook 		= return ()
	}

myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

myManageHook = composeAll									-- xprop | grep WM_CLASS
	[ className	=? "Google-chrome"	--> doShift "2"
	, className 	=? "Java Console"	--> doShift "8"
	, resource 	=? "desktop_window"	--> doIgnore
	, className	=? "Google-chrome"	--> doFloat
	, className 	=? "Java Console"	--> doFloat
	, className 	=? "Galculator"		--> doFloat
	, className 	=? "Steam"		--> doFloat
	, className 	=? "Gimp"		--> doFloat
	, resource 	=? "gpicview"		--> doFloat
	, isFullscreen	--> (doF W.focusDown <+> doFullFloat)]

myLayout = avoidStruts (
	Full |||
--	tabbed shrinkText tabConfig |||
	Tall 1 (3/100) (1/2) |||
	Mirror (Tall 1 (3/100) (1/2))) |||
--	spiral (6/7)) |||
	noBorders (fullscreenFull Full)

colorBlack	= "#020202"
colorGray	= "#7c7c7c"
colorGreen	= "#5bce2d"
colorWhite	= "#eeeeee"
colorBlue	= "#7bb8e2"

tabConfig = defaultTheme 
	{ activeBorderColor	= colorGray
	, activeTextColor	= colorBlue
	, activeColor		= colorBlack
	, inactiveBorderColor	= colorGray
	, inactiveTextColor	= colorWhite
	, inactiveColor		= colorBlack
	}

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
	[ ((modMask, xK_e), spawn $ XMonad.terminal conf)					-- Terminal
	, ((modMask, xK_i), spawn "idea")							-- IntelliJ IDEA
	, ((modMask, xK_g), spawn "google-chrome")						-- Chrome
	, ((modMask, xK_f), spawn "firefox")							-- Firefox
	, ((modMask, xK_x), spawn "xkill")							-- X kill
	, ((modMask .|. shiftMask, xK_l), spawn "gnome-screensaver-command --lock")		-- Lock with gnome
	, ((modMask, xK_d), spawn "dmenu_run -nb '#000000' -nf '#aaaaaa' -sb green -sf black -fn 'terminus-8' -b")
	, ((modMask, xK_q), kill)								-- Close focused window.
	, ((modMask, xK_space), sendMessage NextLayout)						-- Cycle through the available layout algorithms.
	, ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)		-- Reset the layouts on the current workspace to default.
	, ((modMask, xK_n), refresh)								-- Resize viewed windows to the correct size.
	, ((modMask, xK_Tab), windows W.focusDown)						-- Move focus to the next window.
	, ((modMask, xK_j), windows W.focusDown)						-- Move focus to the next window.
	, ((modMask, xK_k), windows W.focusUp)							-- Move focus to the previous window.
	, ((modMask, xK_m), windows W.focusMaster)						-- Move focus to the master window.
	, ((modMask, xK_Return), windows W.swapMaster)						-- Swap the focused window and the master window.
	, ((modMask .|. shiftMask, xK_j), windows W.swapDown)					-- Swap the focused window with the next window.
	, ((modMask .|. shiftMask, xK_k), windows W.swapUp)					-- Swap the focused window with the previous window.
	, ((modMask, xK_h), sendMessage Shrink)							-- Shrink the master area.
	, ((modMask, xK_l), sendMessage Expand)							-- Expand the master area.
	, ((modMask, xK_t), withFocused $ windows . W.sink)					-- Push window back into tiling.
	, ((modMask, xK_comma), sendMessage (IncMasterN 1))					-- Increment the number of windows in the master area.
	, ((modMask, xK_period), sendMessage (IncMasterN (-1)))					-- Decrement the number of windows in the master area.
	, ((modMask .|. shiftMask, xK_q), io (exitWith ExitSuccess))				-- Quit xmonad.
	, ((modMask, xK_r), restart "xmonad" True)						-- Restart xmonad.
	, ((0, xK_Print), spawn "/usr/bin/scrot '%Y-%m-%d_$wx$h.png'") 
	] ++
	[ ((m .|. modMask, k), windows $ f i)
	    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
	    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
	] ++
	[ ((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
	    | (key, sc) <- zip [xK_u, xK_o, xK_p] [0..]
	    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
	]
 
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
	[ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))				-- mod-button1, Set the window to floating mode and move by dragging
	, ((modMask, button2), (\w -> focus w >> windows W.swapMaster))				-- mod-button2, Raise the window to the top of the stack
	, ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))				-- mod-button3, Set the window to floating mode and resize by dragging
	]											-- bind events to the mouse scroll wheel (button4 and button5)
 
