import XMonad
-- Prompt
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Prompt.AppendFile (appendFilePrompt)
import XMonad.Util.EZConfig(additionalKeys)
-- Hooks
import XMonad.Operations
 
import System.IO
import System.Exit
 
import XMonad.Util.Run
 
import XMonad.Actions.CycleWS
import XMonad.Actions.MouseResize
 
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops
 
import XMonad.Layout.NoBorders (smartBorders, noBorders)
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

main = do
        dzenBar <- spawnPipe myBar
        infoPanel <- spawnPipe "/home/koval4/.xmonad/dzen_config.sh"
        xmproc <- spawn "feh --bg-scale --randomize /home/koval4/Pictures/wallpapers/*"
        xmonad $ defaultConfig {
          terminal                 = "xterm"
        , workspaces           = myWorkspaces
        , modMask              = mod4Mask
        , borderWidth          = 2 
        , normalBorderColor  = "#121212"
        , focusedBorderColor = "#71a2df"
        , manageHook = myManageHook --manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts$  mouseResize $ windowArrange $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ renamed [CutWordsLeft 4] $ maximize $ minimize $ boringWindows $ layoutHook defaultConfig
        , logHook             = myLogHook dzenBar
        , keys = myKeys
    }

myBitmapsDir = "/home/kova4/.xmonad/dzen_icons"

myWorkspaces :: [String]
myWorkspaces =  ["web","dev","term","media"] ++ map show [5..9]

myBar = "dzen2 -p -x '0' -y '0' -h '20' -ta 'l' -e 'button2=;' " ++ myBarStyle
myBarStyle = " -fg '#B39D8F' -bg '#121212' " ++ myBarFont
myBarFont = " -fn 'M+1mn:size=8' "
        
-- ManageHook {{{
myManageHook :: ManageHook
myManageHook = (composeAll . concat $
    [ [resource     =? r            --> doIgnore            |   r   <- myIgnores] -- ignore desktop
    , [className    =? c            --> doShift  "dev"    |   c   <- myDev    ] -- move dev to dev 
    , [className    =? c            --> doShift  "web"    |   c   <- myWebs   ] -- move webs to web
    , [className    =? c            --> doShift  "media"  |   c   <- myMedia  ] -- move media to media
    , [className    =? c            --> doCenterFloat       |   c   <- myFloats ] -- float my floats
    , [className    =? c            --> doCenterFloat       |   c   <- myOffice ] -- office to floats
    , [name         =? n            --> doCenterFloat       |   n   <- myNames  ] -- float my names
    , [isFullscreen                 --> myDoFullFloat                           ]
    ]) 
 
    where
 
        role      = stringProperty "WM_WINDOW_ROLE"
        name      = stringProperty "WM_NAME"
 
        -- classnames
        myFloats  = ["Smplayer","MPlayer","VirtualBox","Xmessage","XFontSel","Downloads"]
	myOffice  = ["libreoffice", "libreoffice-startcenter", "libreoffice-writer", "libreoffice-impress", "libreoffice-calc", "libreoffice-draw"]
        myWebs    = ["Firefox","Google-chrome","Chromium", "Chromium-browser"]
        myMedia   = ["rhythmbox", "Vlc"]
        myDev	  = ["QtCreator","codeblocks"]
 
        -- resources
        myIgnores = ["desktop","desktop_window","notify-osd","stalonetray","trayer"]
 
        -- names
        myNames   = ["bashrun","Google Chrome Options","Chromium Options"]
 
-- a trick for fullscreen but stil allow focusing of other WSs
myDoFullFloat :: ManageHook
myDoFullFloat = doF W.focusDown <+> doFullFloat

--Bar
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    {
        ppCurrent           =   dzenColor "#71a2df" "" . wrap " | " " | "
      , ppVisible           =   dzenColor "#B39D8F" "#B39D8F" . pad
      , ppHidden            =   dzenColor "#B39D8F" "" . wrap " " " "
      , ppHiddenNoWindows   =   dzenColor "#444444" "" . wrap " " " "
      , ppUrgent            =   dzenColor "#ff0000" "" . wrap " " " "
      , ppWsSep             =   " "
      , ppSep               =   "  |  "
      , ppLayout            =   dzenColor "#B39D8F" "" . wrap "^ca(1,xdotool key super+space)· " " ·^ca()"
      , ppTitle             =   dzenColor "#B39D8F" ""
                   . wrap "^ca(1,xdotool key super+k)^ca(2,xdotool key super+shift+c)"
                   " ^ca()^ca()" . shorten 50 . dzenEscape
      , ppOutput            =   hPutStrLn h
    }

-- Union default and new key bindings
myKeys x  = M.union (M.fromList (newKeys x)) (keys defaultConfig x)
 
-- Add new and/or redefine key bindings
newKeys conf@(XConfig {XMonad.modMask = modm}) = [
  ((0, xK_Print), spawn "ksnapshot")
  , ((mod1Mask , xK_Shift_L), spawn "/home/koval4/scripts/layout_switch.sh")
  , ((mod4Mask , xK_f), spawn "firefox")
   ]    
    
