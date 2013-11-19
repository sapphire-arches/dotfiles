import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.WorkspaceCompare
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.SimplestFloat
import System.IO
import Data.List

xmobar screen config = spawnPipe . intercalate " " $ options
    where options = [ "xmobar"
                     , "-x"
                     , show screen
                     , show config
                     ]

myManageHook = composeAll
    [ title =? "Fireworks"          --> doFloat
    , title =? "Isomtric Renderer"  --> doFloat
    , title =? "Horse Race"         --> doFloat
    , title =? "Minecraft"          --> doFloat
    , title =? "Minecraft Launcher" --> doFloat
    , title =? "Tekkit"             --> doFloat
    , title =? "Krafty Kat"         --> doFloat
    , title =? "W.o.T. Client"      --> doFloat
    , title =? "Phys Canvas"        --> doFloat
    , title =? "Eclipse"            --> doFloat
    , title =? "testing"            --> doFloat
    , title =? "Defend Rome"        --> doFullFloat
    , title =? "Team Fortress 2 - OpenGL" --> doFullFloat
    , title =? "Nightly"            --> doShift "3"
    , className =? "orage"          --> doFloat
    , className =? "Steam"          --> doShift "9"
    , className =? "Skype"          --> doShift "8"
    , className =? "sun-awt-X11-XFramePeer" --> doIgnore
    , isFullscreen                  --> doFullFloat
    , manageDocks
    ]

myLayoutHook = noBorders $ avoidStrutsOn [D] $ (Full ||| simplestFloat ||| tiled ||| Mirror tiled) where
               tiled = Tall nmaster delta ratio
               nmaster = 1
               delta = 3/100
               ratio = 1/2


main = do 
    xmproc <- Main.xmobar 0 ".xmonad/xmobarrc"
    xmonad $ ewmh defaultConfig
        { terminal      = "terminator"
        , manageHook = myManageHook <+> manageHook defaultConfig
        , layoutHook = myLayoutHook 
        , startupHook = ewmhDesktopsStartup >> setWMName "LG3D"
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "#859900" "" . shorten 100
                        , ppHidden = xmobarColor "#b58900" ""
                        , ppCurrent = wrap "[" "]" . xmobarColor "#dc322f" "" 
                        , ppHiddenNoWindows = xmobarColor "#93a1a1" ""
                        }
        , borderWidth = 2
        , normalBorderColor  = "#268bd2"
        , focusedBorderColor = "#dc322f"
        } `additionalKeys`
        [ ((mod4Mask, xK_z), spawn "xscreensaver-command -lock")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -a")
--        , ((controlMask .|. shiftMask, xK_grave), spawn "wmctrl -a $(wmctrl -l | cut -c 29-79 | awk '{print tolower($0)}'| dmenu)")
        , ((mod1Mask, xK_s), sendMessage $ ToggleStrut R)
        , ((mod1Mask .|. shiftMask, xK_s), sendMessage ToggleStruts)
        , ((0, 0x1008ff13), spawn "amixer -q set Master 5000+") --XF86AudioRaiseVolume
        , ((0, 0x1008ff11), spawn "amixer -q set Master 5000-") --XF86AudioLowerVolume
        , ((0, 0x1008ff12), spawn "amixer -q set Master toggle") --XF86AudioMute
        ]
