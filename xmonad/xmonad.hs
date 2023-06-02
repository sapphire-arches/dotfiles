{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
import XMonad
import XMonad.Actions.UpdatePointer(updatePointer)
import XMonad.Core
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Util.Run(spawnPipe, safeSpawn, runProcessWithInput)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.WorkspaceCompare
import XMonad.Util.Loggers
import XMonad.Util.NamedWindows (unName, getName)
import XMonad.Layout.BorderResize
import XMonad.Layout.Grid
import XMonad.Layout.Gaps
import XMonad.Layout.PositionStoreFloat
import XMonad.Layout.HintedTile hiding (Tall)
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.OneBig
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Operations
import System.IO
import System.Process
import Data.Foldable
import Data.IORef
import Data.List
import Data.Maybe
import Data.Traversable (traverse, fmapDefault)
import qualified XMonad.StackSet as W
import Control.Applicative
import Control.Monad.State.Lazy
import Network.HostName (getHostName)
import XMonad.Hooks.DebugStack

---------
-- Colors
---------
base00 = "#181818"
base01 = "#282828"
base02 = "#383838"
base03 = "#585858"
base04 = "#b8b8b8"
base05 = "#d8d8d8"
base06 = "#e8e8e8"
base07 = "#f8f8f8"
base08 = "#ab4642"
base09 = "#dc9656"
base0A = "#f7ca88"
base0B = "#a1b56c"
base0C = "#86c1b9"
base0D = "#7cafc2"
base0E = "#ba8baf"
base0F = "#a16946"

---------------------
-- Random util stuff
---------------------

safeSplit :: [a] -> (Maybe a, [a])
safeSplit []     = (Nothing, [])
safeSplit (x:xs) = (Just x, xs)

-- Construct an xmobar startup command
xmobar :: ScreenId -> String -> String
xmobar (S screen) config = intercalate " " $ options
    where options = [ "xmobar"
                     , "--screen=" ++ (show screen)
                     , show config
                     ]

writeHandles :: [Handle] -> String -> IO ()
writeHandles h s = forM_ h $ flip hPutStrLn s

floatTitles = ["Fireworks", "Isometric Renderer", "Horse Race", "tile"
              , "Krafty Kat", "W.o.T Client", "Phys Canvas", "testing"
              , "xmessage", "Synth input"]

floatTitleHook = composeAll $ map (\x -> title =? x --> doFloat ) floatTitles

doSendDocks :: ManageHook
doSendDocks = ask >>= \w -> (liftX (trace $ show w)) >> doF id

-- Autofloat some special windows, put things in their place
myManageHook = floatTitleHook <+> composeAll
    [ title =? "Defend Rome"        --> doFullFloat
    , title =? "Team Fortress 2 - OpenGL" --> doFullFloat
    , className =? "orage"          --> doFloat
    , className =? "Steam"          --> doShift (myWorkspaces !! 5)
    , className =? "MPlayer"        --> (ask >>= doF . W.sink)
    , className =? "obsidian"       --> doShift (myWorkspaces !! 6)
    , className =? "sun-awt-X11-XFramePeer" --> doIgnore
    , isFullscreen                  --> hasBorder False
    , checkDock                     --> doLower
    ]

-------------------------------------------------------------------------------
------------------------Custom layout classes----------------------------------
-------------------------------------------------------------------------------

------------------------------
-- Writing mode layout hook --
------------------------------
writtingVerticalFraction :: Rational
writtingVerticalFraction = 1

writtingHorizontalFraction :: Rational
writtingHorizontalFraction = (1/2)

defaultWrittingMode :: WrittingMode Window
defaultWrittingMode = WrittingMode writtingHorizontalFraction writtingVerticalFraction 1

data WrittingMode a = WrittingMode { horizontalFraction :: Rational
                                   , verticalFraction :: Rational
                                   , numFocus :: Int
                                   } deriving (Read, Show)

instance LayoutClass WrittingMode Window where
    pureLayout (WrittingMode horizontal vertical numFocus) rectangle stack = let
        windows = W.integrate stack
        hscale = max 0 (min 1 (horizontal * fromIntegral numFocus))
      in
        runWritting hscale vertical (take numFocus windows) numFocus rectangle
    emptyLayout _ _ = return ([], Nothing)

    pureMessage (WrittingMode horizontal vertical nfocus ) m =
        msum [fmap resize (fromMessage m)
             ,fmap incmastern (fromMessage m)]
      where resize Shrink = WrittingMode (horizontal * (8/9)) vertical nfocus
            resize Expand = WrittingMode (min 1 $ horizontal * (9/8)) vertical nfocus
            incmastern (IncMasterN d) = WrittingMode horizontal vertical (max 1 (nfocus + d))

runWritting :: Rational
            -> Rational
            -> [Window]
            -> Int
            -> Rectangle
            -> [(Window, Rectangle)]
runWritting fracH fracV windows nwindows (Rectangle ix iy iw ih) =
  let
    doFrac start size frac = let
         fsize = fromIntegral size
       in (floor $ fsize * frac, floor $ fromIntegral start + (fsize * (1 - frac) / 2))
    (h, y) = doFrac iy ih fracV
    (w, x) = doFrac ix iw fracH
    scaledrect = Rectangle x y w h
    rects = splitHorizontally nwindows scaledrect
  in
     zip windows rects

data RemoveFocused = RemoveFocused deriving (Read, Show)

instance SetsAmbiguous RemoveFocused where
  hiddens _ wset lr mst wrs = otherHiddens Screen `union` myHiddens
    where
      otherHiddens p = hiddens p wset lr mst wrs
      sr [] = []
      sr [(w, r)] = [w]
      sr (x:y:xs) = []
      myHiddens = sr wrs

myLayoutHook = avoidStrutsOn [L,R,D,U] (lessBorders amb layouts)
    where
        amb = RemoveFocused
        layouts = Full
           ||| tiled
           ||| htile
           ||| simplestFloat
           ||| OneBig (3/4) (3/4)
        tiled = Tall nmaster delta ratio
        htile = borderResize $ HintedTile nmaster delta ratio TopLeft Wide
        nmaster = 1
        delta = 3/100
        ratio = 1/2

-- XMobar output stuff
myTitleLength = 100

formatTitle :: Int -> String -> String
formatTitle _ s = s
-- formatTitle maxLength t
--     | len <  maxLength = t
--     | len == maxLength = t
--     | len >  maxLength = shortenL maxLength t
--     where len = length t;

formatTitles :: Int -> Window -> [(Window, String)] -> String
formatTitles maxLength focused = intercalate " | " . map doFormat
    where doFormat (w, t) = (if w == focused then xmobarColor base0B "" else id)
                                (formatTitle maxLength t)

listWindowTitles :: [Window] -> X [(Window, String)]
listWindowTitles = traverse (fmap getTitle . getName)
    where getTitle w = (unName w, show w)

-- We get the strings in the order: [workspace, layout, current, .. ppExtras ..]
myPPOrder:: [String] -> [String]
myPPOrder xs =
    let
        (a, b) = splitAt 2 xs
    in a ++ drop 1 b

-- Run startup script
doStartup :: IO ProcessHandle
doStartup = do
    (_, _, _, handle) <- createProcess $ shell "~/.xmonad/startup.sh"
    return handle

mySB :: ScreenId -> String -> StatusBarConfig
mySB screen hostname = statusBarProp ("sleep 1") pps
    where
        configFile = (".xmonad/xmobarrc." ++ hostname)
        quote = wrap "\"" "\""
        box cls content = concat [ "(box :class \"", cls, "\" ", content , ")" ]
        formatFocused   _ = box "window-focused" "\"\xea71\""
        formatUnfocused _ = box "window-unfocused" "\"\xeabc\""
        formatFocusedWindow title = concat [
          "(box :class \"window-title\" (label :xalign 0.5 :yalign 0.5 :angle -90 :text \"", take 50 title, "\"))"]
        xmobarPPCfg = def
          { ppSep = ""
          , ppHidden = box "ws-hidden" . quote
          , ppCurrent = box "ws-current" . quote
          , ppVisible = box "ws-visible" . quote
          , ppHiddenNoWindows = box "ws-hidden-no-window" . quote
          , ppLayout = box "layout-name" . quote . singleton . head
          , ppOrder = myPPOrder
          , ppExtras = [ logTitles formatFocused formatUnfocused
                       , logTitles formatFocusedWindow (\_ -> "") ]
          }
        pps = (pure xmobarPPCfg)

barSpawner :: String -> ScreenId -> IO StatusBarConfig
barSpawner hostname i = pure $ mySB i hostname

myWorkspaces :: [String]
myWorkspaces = ["\xf269", "\xf040", "\xf013", "4", "5", "\xf0296", "\xf039a", "\xf001", "\xf1d7"]

-- And the main config
main :: IO ()
main = do
    xmprocs <- newIORef [] :: IO (IORef [Handle])
    hostname <- getHostName
    xmonad $ (docks . ewmh . ewmhFullscreen . (dynamicSBs (barSpawner hostname))) $ def
        { terminal    = "urxvt"
        , manageHook  = myManageHook <+> manageHook def
        , layoutHook  = myLayoutHook
        , modMask     = mod4Mask
        , startupHook = do
          setWMName "LG3D"
          io $ doStartup
          spawn "exec bash -c \"eww -c ~/.config/eww/bar kill; eww -c ~/.config/eww/bar daemon; eww -c ~/.config/eww/bar open bar-l; eww -c ~/.config/eww/bar open bar-r\""
          return ()
        , logHook     = updatePointer (0.5, 0.5) (0.8, 0.8)
        , borderWidth = 2
        , normalBorderColor  = base03
        , focusedBorderColor = base05
        , workspaces = myWorkspaces
        } `additionalKeys`
        [ ((mod4Mask, xK_z), spawn "xscreensaver-command -lock")
        , ((controlMask, xK_Print), spawn "spectacle")
--        , ((controlMask .|. shiftMask, xK_grave), spawn "wmctrl -a $(wmctrl -l | cut -c 29-79 | awk '{print tolower($0)}'| dmenu)")
        , ((mod4Mask .|. shiftMask, xK_s), sendMessage ToggleStruts)
        , ((mod4Mask              , xK_b), withFocused (sendMessage . ResetBorder))
        , ((mod4Mask .|. shiftMask, xK_b), withFocused (sendMessage . (HasBorder False)))
        , ((0, 0x1008ff13), spawn "amixer -D pulse -q set Master 5000+") --XF86AudioRaiseVolume
        , ((0, 0x1008ff11), spawn "amixer -D pulse -q set Master 5000-") --XF86AudioLowerVolume
        , ((0, 0x1008ff12), spawn "amixer -D pulse -q set Master toggle") --XF86AudioMute
        ]
