{-# LANGUAGE DeriveDataTypeable #-}
import XMonad

import System.IO
import Data.Monoid
import qualified Data.Map as M

import XMonad.Util.Run
import XMonad.Util.Loggers
import XMonad.Util.Timer
import qualified XMonad.Util.ExtensibleState as XS

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog

import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.StackTile
import XMonad.Layout.PerWorkspace (onWorkspace)

import XMonad.Actions.WindowGo

import qualified XMonad.StackSet as W

data TIDState = TID TimerId deriving Typeable

instance ExtensionClass TIDState where
  initialValue = TID 0

main :: IO ()
main = do
        dzenBar <- spawnPipe myBar
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
    spawn "feh --bg-scale --randomize /home/koval4/Pictures/ene.png &"
    spawn "mpd &"
    spawn "mpc update &"
    spawn "twmnd &"
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

myBar = "lemonbar -p -d -g 1440x22+0+0 " ++ myBarStyle ++ " | zsh"
myBarStyle = " -F \"#e6f7ff\" -B \"#0b0b14\" " ++ myBarFont
myBarFont = " -f \"M+1mn:size=9\" -f \"FontAwesome:size=9\" "

myLayoutHook = avoidStruts
               $ onWorkspace web webTiled
               $ onWorkspace media mediaTiled
               $ emptyBSP ||| Full
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
        myWebs    = ["vivaldi-snapshot", "Firefox", "Google-chrome", "Chromium", "Chromium-browser","Pidgin","Slack","TelegramDesktop"]
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
    , ((mod4Mask  , xK_F2     ), spawn "rofi -show run")
    , ((mod1Mask  , xK_Shift_L), spawn "/home/koval4/scripts/layout_switch.sh")
    , ((shiftMask , xK_Alt_L  ), spawn "/home/koval4/scripts/layout_switch.sh")
    , ((mod4Mask  , xK_f      ), spawn "firefox")
    , ((mod4Mask  , xK_e      ), spawn "emacs")
    , ((mod4Mask  , xK_b      ), spawn "baka-mplayer")
    , ((mod4Mask  , xK_g      ), spawn "telegram-desktop")
    , ((mod4Mask  , xK_r      ), raiseMaybe (runInTerm "" "ranger")  (resource =? "ranger"))
    , ((mod4Mask  , xK_m      ), raiseMaybe (runInTerm "" "ncmpcpp") (resource =? "ranger"))
    , ((mod1Mask  , xK_Tab    ), windows W.focusUp >> windows W.shiftMaster)
    , ((mod4Mask  , xK_s      ), sendMessage ToggleStruts)
    , ((0         , 0x1008ff13), spawn "amixer set Master 2dB+")
    , ((0         , 0x1008ff11), spawn "amixer set Master 2dB-")
    , ((0         , 0x1008ff14), spawn "mpc toggle")
    , ((0         , 0x1008ff16), spawn "mpc prev")
    , ((0         , 0x1008ff17), spawn "mpc next")
    ]

