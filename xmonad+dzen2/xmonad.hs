{-# LANGUAGE DeriveDataTypeable #-}
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
import Data.Monoid

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
import XMonad.Layout.BoringWindows (boringWindows)
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.Reflect (reflectVert, reflectHoriz, REFLECTX(..), REFLECTY(..))
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), Toggle(..), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import XMonad.Util.Scratchpad (scratchpadSpawnAction, scratchpadManageHook, scratchpadFilterOutWorkspace)
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Timer

import qualified XMonad.StackSet as W
import qualified Data.Map as M

data TIDState = TID TimerId deriving Typeable

instance ExtensionClass TIDState where
  initialValue = TID 0

main :: IO ()
main = do
        dzenBar <- spawnPipe myBar
        --infoPanel <- spawnPipe "/home/koval4/.xmonad/lemonbar_config.sh"
        xmonad $ fullscreenSupport $ def {
          terminal             = "termite"
        , workspaces           = myWorkspaces
        , modMask              = mod4Mask
        , borderWidth          = 4
        , normalBorderColor    = "#0b0b14"
        , focusedBorderColor   = "#71a2df"
        , startupHook          = myStartup
        , manageHook           = manageDocks <+> myManageHook
        , handleEventHook      = mconcat [ docksEventHook, clockEventHook, handleEventHook def ]
        , layoutHook           = myLayoutHook
        , logHook              = myLogHook dzenBar
        , keys = myKeys
        }

myStartup :: X ()
myStartup = do
    spawnOnce "feh --bg-scale --randomize /home/koval4/Pictures/ene.png"
    spawnOnce "mpd"
    spawnOnce "mpc update"
    spawnOnce "twmnd"
    --spawnOnce "/home/koval4/scripts/tasker startReminder /home/koval4/todo.txt"
    startTimer 1 >>= XS.put . TID

clockEventHook :: Event -> X All
clockEventHook e = do
    (TID t) <- XS.get
    handleTimer t e $ do
      startTimer 1 >>= XS.put . TID
      ask >>= logHook . config
      return Nothing
    return $ All True

myWorkspaces :: [String]
myWorkspaces =  [web,dev,term,media,other]

-- Workspaces names
web = "\xf269"
dev = "\xf121"
term = "\xf120"
media = "\xf008"
other = "\xf067"

myBar = "lemonbar -p -d -g 1440x25+0+0 " ++ myBarStyle ++ " | zsh"
myBarStyle = " -F \"#e6f7ff\" -B \"#0b0b14\" " ++ myBarFont
myBarFont = " -f \"M+1mn:size=10\" -f \"FontAwesome:size=10\" "

myLayoutHook = avoidStruts
               -- $ mouseResize
               -- $ windowArrange
               -- $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
               -- $ renamed [CutWordsLeft 4]
               -- $ maximize
               -- $ minimize
               -- $ boringWindows
               $ smartBorders
               $ onWorkspace web webTiled
               $ onWorkspace media mediaTiled
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
    , [className    =? c            --> doShift  dev     |   c   <- myDev    ] -- move dev to dev
    , [className    =? c            --> doShift  web     |   c   <- myWebs   ] -- move webs to web
    , [className    =? c            --> doShift  media   |   c   <- myMedia  ] -- move media to media
    , [className    =? c            --> doCenterFloat    |   c   <- myFloats ] -- float my floats
    , [className    =? c            --> doCenterFloat    |   c   <- myOffice ] -- office to floats
    , [className    =? c            --> doShift other    |   c   <- myBack   ] -- background apps tp 9
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
        myBack    = ["Transmission-gtk"]
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
    , ppVisible           =   wrap "%{F#e6f7ff}" "%{F-}" . pad
    , ppHidden            =   wrap "%{F#e6f7ff}" "%{F-}" . pad
    , ppHiddenNoWindows   =   wrap "%{F#444444}" "%{F-}" . pad
    , ppUrgent            =   wrap "%{F#BA5E57}" "%{F-}" . pad
    , ppWsSep             =   " "
    , ppSep               =   ""
    , ppTitle             =   const ""
    , ppLayout            =   const ""
    , ppExtras            =   [myLog]
    , ppOutput            =   hPutStrLn h
    }
    where
        barColors = "%{B-}%{F-}"

myLog :: Logger
myLog = logCmd "/home/koval4/.xmonad/panel.sh"

-- Union default and new key bindings
myKeys x  = M.union (M.fromList (newKeys x)) (keys def x)

-- Add new and/or redefine key bindings
newKeys conf@XConfig {XMonad.modMask = modm} =
    [ ((0         , xK_Print  ), spawn "spectacle")
    , ((mod1Mask  , xK_Shift_L), spawn "/home/koval4/scripts/layout_switch.sh")
    , ((shiftMask , xK_Alt_L  ), spawn "/home/koval4/scripts/layout_switch.sh")
    , ((mod1Mask  , xK_j      ), spawn "setxkbmap jp")
    , ((mod4Mask  , xK_f      ), spawn "firefox")
    , ((mod4Mask  , xK_q      ), spawn "xmonad --recompile && (pkill lemonbar; xmonad --restart)")
    , ((mod1Mask  , xK_Tab    ), windows W.focusUp >> windows W.shiftMaster)
    , ((mod4Mask  , xK_F2     ), spawn "rofi -show run")
    , ((mod4Mask  , xK_s      ), sendMessage ToggleStruts)
    , ((0         , 0x1008ff13), spawn "amixer set Master 2dB+")
    , ((0         , 0x1008ff11), spawn "amixer set Master 2dB-")
    , ((0         , 0x1008ff14), spawn "mpc toggle")
    , ((0         , 0x1008ff16), spawn "mpc prev")
    , ((0         , 0x1008ff17), spawn "mpc next")
    ]

