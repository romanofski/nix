-- most things copied from:
-- http://haskell.org/haskellwiki/Xmonad/Config_archive/31d1's_xmonad.hs
import XMonad
import Data.List    -- isInfixOf
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.CenteredMaster
import XMonad.Layout.Grid
import XMonad.Layout.MosaicAlt

import XMonad.Util.Run
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.XSelection (safePromptSelection)

import qualified XMonad.Actions.Submap as SM
import qualified XMonad.Actions.Search as S
import XMonad.Prompt
import XMonad.Prompt.Shell

myLayout = avoidStruts $ toggleLayouts full
    (smartBorders (tiled ||| MosaicAlt M.empty ||| centerMaster Grid ||| full))
    where
        full    = noBorders Full
        tiled   = layoutHints $ ResizableTall nmaster delta ratio []
        nmaster = 1
        delta   = 3/100
        ratio   = 1/2


--
-- special windows
-- resource (also known as appName) is the first element in WM_CLASS(STRING) 
-- className is the second element in WM_CLASS(STRING)
-- title is WM_NAME(STRING)
-- http://www.haskell.org/haskellwiki/Xmonad/Frequently_asked_questions#I_need_to_find_the_class_title_or_some_other_X_property_of_my_program
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Aurora"         --> doShift "2:web"
    , className =? "Firefox"        --> doShift "2:web"
    , className =? "Opera"          --> doShift "2:web"
    , className =? "VirtualBox"     --> doShift "3:virtualbox"
    , title     =? "glxgears"       --> doFloat
    , className =? "Pinentry-gtk-2" --> doFloat
    , className =? "XVkbd"          --> doIgnore
    , className =? "Cellwriter"     --> doIgnore
    , className =? "Gtkdialog"      --> doFloat
    , className =? "Qt-recordMyDesktop" --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , isFullscreen                  --> doFullFloat
    --                                      x y w h
    , className =? "Unity-2d-panel" --> doIgnore
    , className =? "Unity-2d-launcher" --> doFloat
    , manageDocks ] <+> manageHook defaultConfig

myWorkspaces = ["1:main", "2:web", "3:virtualbox", "4:whatever"]

myXPConfig = defaultXPConfig { fgColor = "#eee8d5" -- white
                             , bgColor = "#002b36" -- bright black
                             , bgHLight = "#d33682" -- magenta
                             , fgHLight = "#eee8d5"
                             , borderColor = "#002b36"
                             , position = Bottom
                             , font = "xft:Input Mono:size=8"
                             , autoComplete = Just 1
                             }

searchEngineMap method = M.fromList $
  [ ((0, xK_d), method S.duckduckgo)
  , ((0, xK_h), method S.hoogle)
  , ((0, xK_y), method S.youtube)
  , ((0, xK_m), method S.maps)
  , ((0, xK_i), method S.imdb)
  , ((0, xK_w), method S.wikipedia)
  ]

main = do
    xmproc <- spawnPipe "xmobar -x 0"
    xmonad $ docks defaultConfig {
        manageHook           = myManageHook
        , layoutHook         = myLayout
        , terminal           = "termonad"
        , borderWidth        = 3
        , logHook            = dynamicLogString defaultPP >>= xmonadPropLog
        , handleEventHook    = ewmhDesktopsEventHook
        , startupHook        = do
            ewmhDesktopsStartup >> setWMName "LG3D"
            spawn "~/.xmonad/startup-hook"
        , workspaces         = myWorkspaces
  } `additionalKeys`
      [ ((mod1Mask .|. controlMask, xK_l), spawn "loginctl lock-session")
      , ((mod1Mask, xK_p), shellPrompt myXPConfig)
      , ((mod1Mask, xK_b), safePromptSelection "firefox")
      , ((0, xF86XK_AudioRaiseVolume ), spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%")
      , ((0, xF86XK_AudioLowerVolume ), spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%")
      , ((0, xF86XK_AudioMute        ), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
      , ((0, xF86XK_AudioPause     ), spawn "pactl set-source-mute @DEFAULT_SOURCE@ toggle")
      , ((0, xF86XK_MonBrightnessDown     ), spawn "xbacklight -dec 20%")
      , ((0, xF86XK_MonBrightnessUp       ), spawn "xbacklight -inc 20%")
      -- search commands
      , ((mod1Mask, xK_s), SM.submap $ searchEngineMap $ S.promptSearch def)
      , ((mod1Mask .|. shiftMask, xK_s), SM.submap $ searchEngineMap $ S.selectSearch)
      ]
