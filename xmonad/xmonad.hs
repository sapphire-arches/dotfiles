{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
import XMonad
import XMonad.Actions.UpdatePointer(updatePointer)
import XMonad.Core
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.WorkspaceCompare
import XMonad.Util.Loggers
import XMonad.Util.NamedWindows (unName, getName)
import XMonad.Layout.Grid
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.OneBig
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Operations
import System.IO
import System.Process
import Data.IORef
import Data.List
import Data.Maybe
import Data.Traversable (traverse, fmapDefault)
import qualified XMonad.StackSet as W
import Control.Monad.State.Lazy


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

-- Starts an xmobar on a specified screen
xmobar :: Int -> String -> IO (Handle)
xmobar screen config = spawnPipe . intercalate " " $ options
    where options = [ "xmobar"
                     , "-x"
                     , show screen
                     , show config
                     ]

writeHandles :: [Handle] -> String -> IO ()
writeHandles h s = forM_ h $ flip hPutStrLn s

floatTitles = ["Fireworks", "Isometric Renderer", "Horse Race", "tile"
              , "Krafty Kat", "W.o.T Client", "Phys Canvas", "testing"
              , "xmessage", "Synth input"]

floatTitleHook = composeAll $ map (\x -> title =? x --> doFloat ) floatTitles

-- Autofloat some special windows, put things in their place
myManageHook = floatTitleHook <+> composeAll
    [ title =? "Defend Rome"        --> doFullFloat
    , title =? "Team Fortress 2 - OpenGL" --> doFullFloat
    , title =? "Nightly"            --> doShift "3"
    , className =? "orage"          --> doFloat
    , className =? "Steam"          --> doShift "9"
    , className =? "Skype"          --> doShift "8"
    , className =? "MPlayer"        --> (ask >>= doF . W.sink)
    , className =? "sun-awt-X11-XFramePeer" --> doIgnore
    , isFullscreen                  --> doFullFloat
    , manageDocks
    ]

-------------------------------------------------------------------------------
------------------------Custom layout classes----------------------------------
-------------------------------------------------------------------------------

-- Thanks to sjdrodge for his help with making a less derpy implementation of
-- the layout mode

padRect :: Int -> Rectangle -> Rectangle
padRect padding (Rectangle x y w h) =
     let pp = fromIntegral padding -- position padding is a different type than
         pd = fromIntegral padding -- width padding
     in Rectangle ( x + pp ) ( y + pp ) ( w - ( 2 * pd ) ) ( h - ( 2 * pd ) )

-- Takes the current index, total number of rectangles, and the current
-- rectangle and a padding value. Returns a list of rectangles
bspSplit :: Int -> Int -> Rectangle -> [Rectangle]
bspSplit c n rec
    | c == n - 1 = [rec]
    | side <  2  = x : next y
    | otherwise  = y : next x
        where
         side = c `rem` 4
         next = bspSplit (c + 1) n
         (x:y:ys) = (if even side then splitHorizontally else splitVertically) 2 rec

data BinarySplit a = BinarySplit { spacing      :: Int
                                 , spacingDelta :: Int } deriving ( Read, Show )
instance LayoutClass BinarySplit a where
    pureLayout (BinarySplit spacing spacingDelta) rectangle stack = zip windows rectangles
     where
        windows = W.integrate stack
        numWindows = length windows
        rectangles = map ( padRect spacing ) ( bspSplit 0 numWindows rectangle )

    pureMessage (BinarySplit spacing spacingDelta) m =
        msum [fmap resize (fromMessage m)]
      where resize Shrink = BinarySplit ( max 0 $ spacing - spacingDelta ) spacingDelta
            resize Expand = BinarySplit ( spacing + spacingDelta ) spacingDelta

---------------
-- PiP class --
---------------

makeInsetRect (Rectangle x y w h) sw sh = Rectangle x y ow oh
    where ow = floor $ fromIntegral w * sw
          oh = floor $ fromIntegral h * sh

data PictureInPicture a = PictureInPicture {
    insetScaleWidth :: Rational,
    insetScaleHeight :: Rational
} deriving (Read, Show)

instance LayoutModifier PictureInPicture Window where
    modifyLayout (PictureInPicture wr hr) = runPip wr hr
    modifierDescription = show

runPip :: (LayoutClass l Window) =>
                Rational
             -> Rational
             -> W.Workspace WorkspaceId (l Window) Window
             -> Rectangle
             -> X ([(Window, Rectangle)], Maybe (l Window))
runPip scaleW scaleH wksp rect = do
    let stack = W.stack wksp
    let ws = W.integrate' stack
    let (inset, rest) = safeSplit ws
    case inset of
        Just insetWin -> do
            let filteredStack = stack >>= W.filter (insetWin /=)
            let pipRect = makeInsetRect rect scaleW scaleH
            wrs <- runLayout (wksp {W.stack = filteredStack}) rect
            return ((insetWin, pipRect) : fst wrs, snd wrs)
        Nothing -> runLayout wksp rect


withPip s = ModifiedLayout $ PictureInPicture s s
withPipSeparate w h = ModifiedLayout $ PictureInPicture w h

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

layoutWrapper :: (LayoutClass l Window) =>
                 l Window -> ModifiedLayout WithBorder (ModifiedLayout AvoidStruts l) Window
layoutWrapper = noBorders . avoidStrutsOn [D,U]

myLayoutHook = layoutWrapper (Full
                             ||| tiled
                             ||| bsplit
                             ||| simplestFloat
                             ||| withPip (1/3) bsplit
                             ||| withPipSeparate (2/3) (1/6) Full
                             ||| Mirror tiled
                             ||| OneBig (3/4) (3/4)) where
                 bsplit = BinarySplit 8 6
                 tiled = Tall nmaster delta ratio
                 nmaster = 1
                 delta = 3/100
                 ratio = 1/2

-- XMobar output stuff
myTitleLength = 100

formatTitle :: Int -> String -> String
formatTitle maxLength t
    | len <  maxLength = t
    | len == maxLength = t
    | len >  maxLength = shorten maxLength t
    where len = length t;

formatTitles :: Int -> Window -> [(Window, String)] -> String
formatTitles maxLength focused = intercalate " | " . map doFormat
    where doFormat (w, t) = (if w == focused then xmobarColor base0B "" else id)
                                (formatTitle maxLength t)

listWindowTitles :: [Window] -> X [(Window, String)]
listWindowTitles = traverse (fmap getTitle . getName)
    where getTitle w = (unName w, show w)

logTitles :: X ( Maybe String )
logTitles = withWindowSet $ formatStackSet
    where
        formatStackSet :: WindowSet -> X (Maybe String)
        formatStackSet s =
            let
                windows       = W.index s
                titles        = listWindowTitles windows
            -- Outer fmap is to extract titles, since that needs to be in the X monad
            in fmap (\wintitles -> fmap (\focused ->
                -- Inner fmap is just convenience over Maybe
                let
                    numWindows    = length windows
                    desiredLength = min (quot 200 numWindows) myTitleLength
                in formatTitles desiredLength focused wintitles) (W.peek s)
            ) titles

-- Utility function for setting the font
xmobarFont :: Int -> String -> String
xmobarFont i = wrap (concat ["<fn=", show i, ">"]) "</fn>"

-- We get the strings in the order: [workspace, layout, current, .. ppExtras ..]
myPPOrder:: [String] -> [String]
myPPOrder xs =
    let
        (a, b) = splitAt 2 xs
    in a ++ drop 1 b

doStartup :: IO ProcessHandle
doStartup = do
    (_, _, _, handle) <- createProcess $ shell "~/.xmonad/startup.sh"
    return handle

myWorkspaces = ["\xf269", "\xf040", "\xf013", "4", "5", "6", "7", "\xf001", "\xf1d7"]

-- And the main config
main :: IO ()
main = do
    xmprocs <- newIORef [] :: IO (IORef [Handle])
    doStartup
    Main.xmobar 0 ".xmonad/xmobarrc" >>= (\x -> modifyIORef xmprocs ( x : ) )
    xmonad $ ewmh defaultConfig
        { terminal      = "urxvt"
        , manageHook = myManageHook <+> manageHook defaultConfig
        , handleEventHook = docksEventHook
        , layoutHook = myLayoutHook
        , modMask = mod4Mask
        , startupHook = do ewmhDesktopsStartup
                           setWMName "LG3D"
                           screens <- withDisplay getCleanedScreenInfo
                           bars <- io . sequence $ take ( ( length screens ) - 1 ) ( map (flip Main.xmobar ".xmonad/xmobar-secondaryrc") [1..] )
                           io $ modifyIORef xmprocs ( bars ++ )
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = (\x -> readIORef xmprocs >>= flip writeHandles x)
                        , ppSep = " \xb7 "
                        , ppHidden = xmobarColor base09 ""
                        , ppCurrent = xmobarColor base08 ""
                        , ppHiddenNoWindows = xmobarColor base02 ""
                        , ppExtras = [ logTitles ]
                        , ppLayout = (head . words)
                        , ppOrder = myPPOrder
                        } >>
                    updatePointer (0.5, 0.5) (0.8, 0.8)
        , borderWidth = 2
        , normalBorderColor  = "#268bd2"
        , focusedBorderColor = "#dc322f"
        , workspaces = myWorkspaces
        } `additionalKeys`
        [ ((mod4Mask, xK_z), spawn "xscreensaver-command -lock")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -a")
--        , ((controlMask .|. shiftMask, xK_grave), spawn "wmctrl -a $(wmctrl -l | cut -c 29-79 | awk '{print tolower($0)}'| dmenu)")
        , ((mod4Mask, xK_f), setLayout $ Layout $ layoutWrapper $ defaultWrittingMode)
        , ((mod4Mask, xK_s), sendMessage $ ToggleStrut R)
        , ((mod4Mask .|. shiftMask, xK_s), sendMessage ToggleStruts)
        , ((0, 0x1008ff13), spawn "amixer -D pulse -q set Master 5000+") --XF86AudioRaiseVolume
        , ((0, 0x1008ff11), spawn "amixer -D pulse -q set Master 5000-") --XF86AudioLowerVolume
        , ((0, 0x1008ff12), spawn "amixer -D pulse -q set Master toggle") --XF86AudioMute
        ]
