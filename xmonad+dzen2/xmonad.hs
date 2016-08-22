import XMonad
-- Prompt
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Prompt.AppendFile (appendFilePrompt)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.SpawnOnce
import XMonad.Operations

import System.IO
import System.Exit

import XMonad.Util.Run
import XMonad.Util.Loggers

import XMonad.Actions.CycleWS
import XMonad.Actions.MouseResize

-- Hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops

--Layouts
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.IM
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Renamed (renamed, Rename(CutWordsLeft, Replace))
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Grid
import XMonad.Layout.Minimize
import XMonad.Layout.Maximize
import XMonad.Util.Scratchpad (scratchpadSpawnAction, scratchpadManageHook, scratchpadFilterOutWorkspace)
import XMonad.Layout.BoringWindows (boringWindows)
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.Reflect (reflectVert, reflectHoriz, REFLECTX(..), REFLECTY(..))
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), Toggle(..), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))

import qualified XMonad.StackSet as W
import qualified Data.Map as M

myStartup = do
    spawnOnce "feh --bg-scale --randomize /home/koval4/Pictures/wallhaven-4145.png"
    spawnOnce "mpd"
    spawnOnce "mpc update"
    spawnOnce "twmnd"
    spawnOnce "$HOME/scripts/tasker startReminder $HOME/todo.txt"

main = do
        dzenBar <- spawnPipe myBar
        infoPanel <- spawnPipe "/home/koval4/.xmonad/dzen_config.sh"
        xmonad $ fullscreenSupport $ def {
          terminal             = "termite"
        , workspaces           = myWorkspaces
        , modMask              = mod4Mask
        , borderWidth          = 4
        , normalBorderColor    = bg
        , focusedBorderColor   = "#71a2df"
        , startupHook          = myStartup
        , manageHook           = myManageHook --manageDocks <+> manageHook defaultConfig
        , handleEventHook      = mconcat [ docksEventHook, handleEventHook def ]
        , layoutHook           = myLayoutHook
        , logHook              = myLogHook dzenBar
        , keys = myKeys
        }

myBitmapsDir = "/home/kova4/.xmonad/dzen_icons/"
bg = "#0b0b14"

myWorkspaces :: [String]
myWorkspaces =  ["web","dev","term","media","other"]

myBar = "lemonbar -p -d -g 1440x25+0+0 " ++ myBarStyle
myBarStyle = " -F \"#e6f7ff\" -B \"#0b0b14\" " ++ myBarFont
myBarFont = " -f \"M+1mn:size=10\" "

myLayoutHook = avoidStruts
               -- $ mouseResize
               -- $ windowArrange
               -- $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
               -- $ renamed [CutWordsLeft 4]
               -- $ maximize
               -- $ minimize
               -- $ boringWindows
               $ smartBorders
               $ onWorkspace "web" webTiled
               $ onWorkspace "media" mediaTiled
               $ tiled ||| Full
               where tiled = Tall nmaster delta ratio
                     mediaTiled = noBorders $ tiled
                     webTiled = Tall nmaster delta webRatio
                     nmaster = 1
                     ratio = 3/5
                     webRatio = 5/7
                     delta = 3/100

-- ManageHook {{{
myManageHook :: ManageHook
myManageHook = (composeAll . concat $
    [ [resource     =? r            --> doIgnore         |   r   <- myIgnores] -- ignore desktop
    , [className    =? c            --> doShift  "dev"   |   c   <- myDev    ] -- move dev to dev
    , [className    =? c            --> doShift  "web"   |   c   <- myWebs   ] -- move webs to web
    , [className    =? c            --> doShift  "media" |   c   <- myMedia  ] -- move media to media
    , [className    =? c            --> doCenterFloat    |   c   <- myFloats ] -- float my floats
    , [className    =? c            --> doCenterFloat    |   c   <- myOffice ] -- office to floats
    , [className    =? c            --> doShift "other"  |   c   <- myBack   ] -- background apps tp 9
    , [name         =? n            --> doCenterFloat    |   n   <- myNames  ] -- float my names
    , [isFullscreen                 --> myDoFullFloat                        ]
    ]) where
        name      = stringProperty "WM_NAME"
        -- classnames
        myFloats  = ["VirtualBox","Xmessage","XFontSel","Downloads","feh","Pidgin"]
        myOffice  = ["libreoffice", "libreoffice-startcenter", "libreoffice-writer", "libreoffice-impress", "libreoffice-calc", "libreoffice-draw"]
        myWebs    = ["vivaldi-snapshot", "Firefox", "Google-chrome", "Chromium", "Chromium-browser","Pidgin","Slack","telegram-desktop"]
        myMedia   = ["rhythmbox", "Vlc", "baka-mplayer"]
        myDev     = ["QtCreator", "codeblocks", "MonoDevelop", "Emacs"]
        myBack    = ["transmission"]
        -- resources
        myIgnores = ["desktop","desktop_window","notify-osd","stalonetray","trayer","conky", "plank"]
        -- names
        myNames   = ["bashrun","Google Chrome Options","Chromium Options"]

-- a trick for fullscreen but stil allow focusing of other WSs
myDoFullFloat :: ManageHook
myDoFullFloat = doF W.focusDown <+> doFullFloat

--Bar
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ XMonad.Hooks.DynamicLog.def
    { ppCurrent           =   wrap "%{B#71a2df}%{F#121212}" barColors . pad
    , ppVisible           =   wrap "%{F#e6f7ff}" "%{F#e6f7ff}" . pad
    , ppHidden            =   wrap "%{F#e6f7ff}" "%{F#e6f7ff}" . pad
    , ppHiddenNoWindows   =   wrap "%{F#444444}" "" . pad
    , ppUrgent            =   wrap "%{F#BA5E57}" "" . pad
    , ppWsSep             =   " "
    , ppSep               =   "  |  "
    , ppTitle             =   const ""
    , ppLayout            =   const ""
    -- , ppExtras            =   [myLog]
    , ppOutput            =   hPutStrLn h
    }
    where
        barColors = "%{B#0b0b14}%{F#e6f7ff}"

myLog :: Logger
myLog = logCmd "/home/koval4/.xmonad/panel.sh"

-- Union default and new key bindings
myKeys x  = M.union (M.fromList (newKeys x)) (keys def x)

-- Add new and/or redefine key bindings
newKeys conf@XConfig {XMonad.modMask = modm} =
    [ ((0, xK_Print           ), spawn "spectacle")
    , ((mod1Mask  , xK_Shift_L), spawn "/home/koval4/scripts/layout_switch.sh")
    , ((shiftMask , xK_Alt_L  ), spawn "/home/koval4/scripts/layout_switch.sh")
    , ((mod1Mask  , xK_j      ), spawn "setxkbmap jp")
    , ((mod4Mask  , xK_f      ), spawn "firefox")
    , ((mod4Mask  , xK_q      ), spawn "pkill dzen2 && xmonad --restart")
    , ((mod1Mask  , xK_Tab    ), windows W.focusUp >> windows W.shiftMaster)
    , ((mod4Mask  , xK_F2     ), spawn "rofi -show run")
    , ((mod4Mask  , xK_s      ), sendMessage ToggleStruts)
    ]

