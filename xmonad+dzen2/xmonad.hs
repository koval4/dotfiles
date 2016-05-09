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
    --spawnOnce "compton -b"
    spawnOnce "feh --bg-scale --randomize /home/koval4/Pictures/wallhaven-4145.png"
    --spawnOnce "apulse skype"
    spawnOnce "mpd"
    spawnOnce "mpc update"
    --spawnOnce "plank"
    spawnOnce "twmnd"

main = do
        dzenBar <- spawnPipe myBar
        infoPanel <- spawnPipe "/home/koval4/.xmonad/dzen_config.sh"
        xmonad $ def { 
          terminal             = "termite"
        , workspaces           = myWorkspaces
        , modMask              = mod4Mask
        , borderWidth          = 4 
        , normalBorderColor    = bg
        , focusedBorderColor   = "#71a2df"
        , startupHook          = myStartup
        , manageHook           = myManageHook --manageDocks <+> manageHook defaultConfig
        , layoutHook           = myLayoutHook
        , logHook              = myLogHook dzenBar
        , keys = myKeys
        }

myBitmapsDir = "/home/kova4/.xmonad/dzen_icons/"
bg = "#0b0b14"

myWorkspaces :: [String]
myWorkspaces =  ["web","dev","term","media","other"]

myBar = "dzen2 -p -x '0' -y '0' -h '20' -ta 'l' -e 'button2=;' " ++ myBarStyle
myBarStyle = " -fg '#e6f7ff' -bg '#0b0b14' " ++ myBarFont
myBarFont = " -fn 'M+1mn:size=10' "

myLayoutHook = avoidStruts 
               $ onWorkspace "web" simpleFloat
               $ mouseResize 
               $ windowArrange 
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) 
               $ renamed [CutWordsLeft 4] 
               $ maximize 
               $ minimize 
               $ boringWindows 
               $ lessBorders OnlyFloat 
               $ tiled ||| Full
               where tiled = Tall nmaster delta ratio
                     nmaster = 1
                     ratio = 3/5
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
        myFloats  = ["VirtualBox","Xmessage","XFontSel","Downloads","feh","Pidgin"] ++ myWebs ++ myOffice
        myOffice  = ["libreoffice", "libreoffice-startcenter", "libreoffice-writer", "libreoffice-impress", "libreoffice-calc", "libreoffice-draw"]
        myWebs    = ["vivaldi-snapshot", "Firefox", "Google-chrome", "Chromium", "Chromium-browser","Pidgin","Slack","telegram-desktop"]
        myMedia   = ["rhythmbox", "Vlc", "baka-mplayer"]
        myDev     = ["QtCreator", "codeblocks"]
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
myLogHook h = dynamicLogWithPP $ def
    { ppCurrent           =   dzenColor "#121212" "#71a2df" . pad
    , ppVisible           =   dzenColor "#e6f7ff" "#e6f7ff" . pad
    , ppHidden            =   dzenColor "#e6f7ff" "" . pad
    , ppHiddenNoWindows   =   dzenColor "#444444" "" . pad
    , ppUrgent            =   dzenColor "#BA5E57" "" . pad
    , ppWsSep             =   " "
    , ppSep               =   "  |  "
    , ppLayout            =   dzenColor "#e6f7ff" "" . 
                              wrap "^ca(1,xdotool key super+space)" "^ca()" .
                              (\x -> case x of 
                                  "Simple Float"    -> wrapBitmap "nbstack.xbm"
                                  "Tall"            -> wrapBitmap "layout_tall.xbm"
                                  "Full"            -> wrapBitmap "layout_full.xbm"
                                  _                 -> wrapBitmap "tile.xbm"
                              )
    , ppTitle             =   dzenColor "#e6f7ff" ""
                 . wrap "^ca(1,xdotool key super+k)^ca(2,xdotool key super+shift+c)"
                 " ^ca()^ca()" . shorten 60 . dzenEscape
    , ppOutput            =   hPutStrLn h
    }
    where
        wrapBitmap bitmap = "^i(/home/koval4/.xmonad/dzen_icons/" ++ bitmap ++ ")"

-- Union default and new key bindings
myKeys x  = M.union (M.fromList (newKeys x)) (keys def x)
 
-- Add new and/or redefine key bindings
newKeys conf@XConfig {XMonad.modMask = modm} = 
    [ ((0, xK_Print           ), spawn "spectacle")
    , ((mod1Mask  , xK_Shift_L), spawn "/home/koval4/scripts/layout_switch.sh")
    , ((shiftMask , xK_Alt_L  ), spawn "/home/koval4/scripts/layout_switch.sh")
    , ((mod4Mask  , xK_f      ), spawn "firefox")
    , ((mod4Mask  , xK_q      ), spawn "pkill dzen2 && xmonad --restart")
    , ((mod1Mask  , xK_Tab    ), windows W.focusUp >> windows W.shiftMaster)
    , ((mod4Mask  , xK_F2     ), spawn "rofi -show run")
    ]    
    
