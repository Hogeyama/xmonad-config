{-# LANGUAGE FlexibleContexts #-}

import XMonad
import XMonad.Util.Run (spawnPipe, hPutStrLn)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Layout.LayoutModifier (ModifiedLayout)

import Data.Default

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar $HOME/.xmonad/.xmobarrc"
  xmonad $ def
       { modMask = mod4Mask
       , terminal = "gnome-terminal"
       , borderWidth = 3
       , focusedBorderColor = "#00bfff"
       , manageHook = manageDocks <+> manageHook def
       , layoutHook = avoidStruts  $  layoutHook def
       , logHook = dynamicLogWithPP xmobarPP
                       { ppOutput = hPutStrLn xmproc
                       , ppTitle = xmobarColor "green" "" . shorten 50
                       }
       }

