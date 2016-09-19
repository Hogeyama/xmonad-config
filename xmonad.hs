{-# LANGUAGE FlexibleContexts #-}

import XMonad
import XMonad.Operations            (kill)

import XMonad.Util.Run              (spawnPipe, hPutStrLn)
import XMonad.Hooks.ManageDocks     (manageDocks, avoidStruts)
import XMonad.Hooks.DynamicLog      (xmobarPP, xmobarColor, shorten, dynamicLogWithPP, PP(..))
import XMonad.Util.EZConfig         (additionalKeys, additionalKeysP, removeKeysP)
import XMonad.Layout.StackTile

import qualified XMonad.StackSet as W

import Data.Default

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar $HOME/.xmonad/.xmobarrc"
  xmonad $ def
      { modMask            = mod4Mask
      , terminal           = "gnome-terminal"
      , workspaces         = myWorkspaces
      , borderWidth        = 2
      , focusedBorderColor = "#00bfff"
      , manageHook         = manageDocks <+> manageHook def
      , layoutHook         = avoidStruts myLayout
      , logHook            = dynamicLogWithPP xmobarPP
                                { ppOutput = hPutStrLn xmproc
                                , ppTitle = xmobarColor "green" "" . shorten 50
                                }
      , startupHook        = spawn "dropbox start"
      }

      `additionalKeysP`
      [ ("M-g"  , spawn "google-chrome")
      , ("M-p"  , spawn "gmrun")
      , ("M-S-p", spawn "dmenu_run")
      , ("M-S-q", kill)
      ]

      `additionalKeysP`
      [ ("<XF86AudioRaiseVolume>"  , spawn "amixer -D pulse sset Master 2%+")
      , ("<XF86AudioLowerVolume>"  , spawn "amixer -D pulse sset Master 2%-")
      , ("<XF86AudioMute>"         , spawn "amixer -D pulse sset Master 0%" )
      , ("M-<XF86AudioRaiseVolume>", spawn "xbacklight -inc 10")
      , ("M-<XF86AudioLowerVolume>", spawn "xbacklight -dec 10")
      , ("M-<XF86AudioMute>"       , spawn $ "scrot " ++ screenShotName)
      ]

      `additionalKeys`
      [((m .|. mod4Mask, k), windows $ f i)
          | (i, k) <- zip (tail myWorkspaces) [xK_1 .. xK_9]
          , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

screenShotName :: String
screenShotName = "$HOME/Dropbox/ScreenShots/Screenshot%Y-%m-%d-%H:%M:%S.png"

myLayout = tiled ||| Full
  where
     tiled   = Tall      nmaster delta ratioT
     stack   = StackTile nmaster delta ratioS
     nmaster = 1
     delta   = 3/100
     ratioT  = 1/2
     ratioS  = 4/5

myWorkspaces = "dummy" : map show [1..9 :: Int]

