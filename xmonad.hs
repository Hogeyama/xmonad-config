{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

{- {{{ -}
import RIO hiding (Const)
import RIO.List qualified as List

import Data.List.Split qualified as List
import GHC.IO.Encoding (TextEncoding)
import System.IO (localeEncoding)
import System.IO qualified as IO
import System.Posix.IO (
  FdOption (CloseOnExec),
  closeFd,
  createPipe,
  dupTo,
  fdToHandle,
  setFdOption,
  stdInput,
 )
import System.Posix.Process (executeFile)

import XMonad
import XMonad.Hooks.DynamicLog (
  PP (..),
  dynamicLogString,
  statusBar,
  xmobarPP,
 )
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks (
  AvoidStruts,
  ToggleStruts (..),
  manageDocks,
 )
import XMonad.Layout.ComboP (
  CombineTwoP,
  Property (..),
  SwapWindow (..),
  combineTwoP,
 )
import XMonad.Layout.Decoration (
  Decoration,
  DefaultShrinker,
 )
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.Simplest (Simplest)
import XMonad.Layout.Tabbed (
  TabbedDecoration,
  Theme (..),
  shrinkText,
  tabbed,
 )
import XMonad.Layout.TwoPane (TwoPane (..))
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig (
  additionalKeys,
  additionalKeysP,
  removeKeysP,
 )
import XMonad.Util.Run (
  runProcessWithInput,
  safeSpawn,
 )

{- }}} -}

main :: IO ()
main = xmonad =<< xmobar' (ewmh myConfig)
  where
    myConfig =
      def
        { modMask = mod4Mask
        , terminal = "gnome-terminal"
        , workspaces = myWorkspaces
        , focusedBorderColor = "#00FF00"
        , normalBorderColor = "#EEEEEE"
        , manageHook = manageDocks <+> manageHook def
        , layoutHook = myLayoutHook
        , startupHook =
            mapM_
              spawn
              [ "dropbox start"
              , "unity-settings-daemon"
              , "compton -CG --active-opacity 1.0 --shadow-ignore-shaped"
              , "feh --bg-scale $HOME/Dropbox/WallPapers/Reflexion.jpg"
              , "xmodmap $HOME/.Xmodmap"
              ]
        , handleExtraArgs = \xs conf -> do
            mborder <- tryAnyDeep $ read <$> readFile "/tmp/xmonad_borderwidth"
            handleExtraArgs
              def
              xs
              conf
                { borderWidth = case mborder of
                    Right x -> x
                    Left _ -> 2
                }
        }
        `additionalKeysP`
        -- [ ("M-g"          , spawn "google-chrome")
        [ ("M-g", spawn "firefox")
        , ("M-p", spawn "ulauncher")
        , ("M-S-q", kill)
        , ("M-S-C-q", io exitSuccess)
        , ("M-x", spawn "sudo pm-suspend")
        , ("M-S-x", spawn "systemctl suspend")
        , ("M-<Space>", toggleTwoPane)
        , ("M-S-<Space>", setLayoutType LayoutFull)
        , ("M-<Return>", focusNextScreen)
        , ("M-C-<Return>", shiftNextScreen)
        , ("M-s", swapScreen)
        , ("M-a", sendMessage SwapWindow)
        , ("M-S-a", hoge) -- なんか動作の確認に
        -- , ("M-S-d"        , killXmobar)
        , ("M-S-r", restart "xmonad" True)
        , ("M-k", focusUpOrAnotherPane)
        , ("M-j", focusDownOrAnotherPane)
        , ("M-S-k", focusUp)
        , ("M-S-j", focusDown)
        , -- , ("M-S-k"        , spawn "amixer -D pulse sset Master 2%+")
          -- , ("M-S-j"        , spawn "amixer -D pulse sset Master 2%-")
          ("M-S-o", spawn "amixer -D pulse sset Master mute")
        , ("M-S-t", spawn "amixer -D pulse sset Master toggle")
        , ("M-S-s", spawn $ unwords ["scrot ", screenShotName])
        , ("M-S-b", spawn "$HOME/.local/bin/bttoggle")
        , ("M-m", toggleTouchPad)
        , ("M-b", sendMessage ToggleStruts) -- xmobar
        ]
        `additionalKeysP` [ ("<XF86AudioRaiseVolume>", spawn "amixer -D pulse sset Master 2%+")
                          , ("<XF86AudioLowerVolume>", spawn "amixer -D pulse sset Master 2%-")
                          , ("<XF86AudioMute>", spawn "amixer -D pulse sset Master 0%")
                          , ("M-<XF86AudioRaiseVolume>", spawn "xbacklight -inc 10")
                          , ("M-<XF86AudioLowerVolume>", spawn "xbacklight -dec 10")
                          ]
        `additionalKeys` [ ((m .|. mod4Mask, k), windows $ f i)
                         | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
                         , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
                         ]
        `removeKeysP` ["S-C-n"]

    screenShotName :: String
    screenShotName = "$HOME/Dropbox/ScreenShots/Screenshot%Y-%m-%d-%H-%M-%S.png"

    myWorkspaces :: [String]
    myWorkspaces = map show [1 .. 9 :: Int]

-------------------------------------------------------------------------------
-- xmobar
-------------------------------------------------------------------------------

xmobar' ::
  LayoutClass l Window =>
  XConfig l ->
  IO (XConfig (ModifiedLayout AvoidStruts l))
xmobar' = statusBar cmd xmobarPP {ppLayout = ppLayout'} defToggleStrutsKey
  where
    cmd = "$HOME/.xmonad/xmobar-x86_64-linux"
    -- cmd = "$HOME/.xmonad/result/bin/xmobar-x86_64-linux"
    ppLayout' s = case parseLayoutType s of
      LayoutFull -> "Full"
      LayoutTabbed -> "Tabbed"
      LayoutTwoPaneTabbed -> "TwoPane"

-- | Default @mod-b@ key binding for 'withEasySB'
defToggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
defToggleStrutsKey XConfig {modMask = modm} = (modm, xK_b)

-------------------------------------------------------------------------------
-- Layout
-------------------------------------------------------------------------------

type (:$) = ModifiedLayout
type (:|) = Choose
infixr 6 :$
infixr 5 :|
type SimpleTab = Decoration TabbedDecoration DefaultShrinker :$ Simplest

type MyLayoutHook =
  SimpleTab
    :| CombineTwoP (TwoPane ()) SimpleTab SimpleTab
    :| Full

data LayoutType
  = LayoutFull
  | LayoutTabbed
  | LayoutTwoPaneTabbed
  deriving (Eq, Ord, Show)

myLayoutHook :: MyLayoutHook Window
myLayoutHook =
  myTabbed
    ||| combineTwoP (TwoPane (1 / 50) (1 / 2)) myTabbed myTabbed (Const True)
    ||| Full
  where
    myTabbed =
      tabbed
        shrinkText
        def
          { activeColor = "#1A1E1B"
          , activeTextColor = "#00FF00"
          , activeBorderColor = "#000000"
          , inactiveColor = "#1A1E1B"
          , inactiveTextColor = "#676767"
          , inactiveBorderColor = "#000000"
          , activeBorderWidth = 1
          , inactiveBorderWidth = 1
          , fontName = "xft:Rounded Mgen+ 1mn:size=8"
          , decoHeight = 30
          }

hoge :: X ()
hoge = do
  log' "nyan"

log' :: MonadIO m => String -> m ()
log' s = liftIO $ appendFile "/home/hogeyama/xmonad.mylog" (s <> "\n")

-------------------------------------------------------------------------------
-- Command
-------------------------------------------------------------------------------

focusNextScreen :: X ()
focusNextScreen = withNextScreen W.view

shiftNextScreen :: X ()
shiftNextScreen = withNextScreen W.shift

swapScreen :: X ()
swapScreen = windows $ \stack -> case W.visible stack of
  [] -> stack
  x : rest ->
    stack
      { W.current = y {W.workspace = W.workspace x}
      , W.visible = x {W.workspace = W.workspace y} : rest
      }
    where
      y = W.current stack

toggleTouchPad :: X ()
toggleTouchPad = setTouchPad . not =<< isTouchPadEnabled
  where
    setTouchPad :: Bool -> X ()
    setTouchPad b =
      safeSpawn
        "gsettings"
        [ "set"
        , "org.gnome.desktop.peripherals.touchpad"
        , "send-events"
        , if b then "enabled" else "disabled"
        ]
    isTouchPadEnabled :: X Bool
    isTouchPadEnabled = do
      out <-
        runProcessWithInput
          "gsettings"
          [ "get"
          , "org.gnome.desktop.peripherals.touchpad"
          , "send-events"
          ]
          ""
      case out of
        "'enabled'\n" -> pure True
        "'disabled'\n" -> pure False
        _ -> error' $ "toggleTouchPad: unknown input: " <> show out
      where
        error' s = log' s >> error s

-- touchpad=$(gsettings list-schemas | grep touchpad)
-- gsettings list-keys $touchpad
-- gsettings range $touchpad some-key

toggleTwoPane :: X ()
toggleTwoPane =
  getCurrentLayoutType >>= \case
    LayoutFull -> setLayoutType LayoutTabbed
    LayoutTabbed -> setLayoutType LayoutTwoPaneTabbed
    LayoutTwoPaneTabbed -> setLayoutType LayoutTabbed

focusUp :: X ()
focusUp =
  getCurrentLayoutType >>= \case
    LayoutTwoPaneTabbed -> focusUpInPane
    _ -> windows W.focusUp

focusDown :: X ()
focusDown =
  getCurrentLayoutType >>= \case
    LayoutTwoPaneTabbed -> focusDownInPane
    _ -> windows W.focusDown

focusUpOrAnotherPane :: X ()
focusUpOrAnotherPane =
  getCurrentLayoutType >>= \case
    LayoutTwoPaneTabbed -> focusAnotherPane
    _ -> windows W.focusUp

focusDownOrAnotherPane :: X ()
focusDownOrAnotherPane =
  getCurrentLayoutType >>= \case
    LayoutTwoPaneTabbed -> focusAnotherPane
    _ -> windows W.focusDown

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

getCurrentLayoutType :: X LayoutType
getCurrentLayoutType = parseLayoutType <$> getCurrentLayoutName

getCurrentLayoutName :: X String
getCurrentLayoutName = dynamicLogString def {ppOrder = \ ~[_, l, _] -> [l]}

parseLayoutType :: String -> LayoutType
parseLayoutType s
  | "combining" `List.isPrefixOf` s = LayoutTwoPaneTabbed
  | "Tabbed" `List.isPrefixOf` s = LayoutTabbed
  | otherwise = LayoutFull

setLayoutType :: LayoutType -> X ()
setLayoutType t = do
  t' <- getCurrentLayoutType
  unless (t == t') $ do
    sendMessage NextLayout
    setLayoutType t

withNextScreen :: (WorkspaceId -> WindowSet -> WindowSet) -> X ()
withNextScreen func =
  gets (W.visible . windowset) >>= \case
    [] -> pure ()
    next : _ -> windows $ func $ W.tag $ W.workspace next

-- XXX ad hoc
focusAnotherPane :: X ()
focusAnotherPane =
  getPanesInfo >>= \case
    Just (all', _focusedPane, unfocusedPane) -> do
      let mVisibleOnUnfocusedPane =
            -- 多分あってる
            List.find (`elem` unfocusedPane) all'
      case mVisibleOnUnfocusedPane of
        Nothing -> log' "UnfocusedPane is empty"
        Just v -> focus v
    Nothing -> pure ()

focusUpInPane :: X ()
focusUpInPane =
  getPanesInfo >>= \case
    Just (_all', focusedPane, _unfocusedPane) -> do
      getFocusedWin >>= \case
        Just focused -> do
          let x = reverse focusedPane
          focus $ dropWhile (/= focused) (x ++ x) !! 1
        Nothing -> pure ()
    Nothing -> pure ()

focusDownInPane :: X ()
focusDownInPane =
  getPanesInfo >>= \case
    Just (_all', focusedPane, _unfocusedPane) -> do
      getFocusedWin >>= \case
        Just focused -> do
          let x = reverse focusedPane
          focus $ dropWhile (/= focused) (x ++ x) !! 1
        Nothing -> pure ()
    Nothing -> pure ()

-- XXX ad hoc
-- returns (All Windows, Forcused Pane, UnfocusedPane)
getPanesInfo :: X (Maybe ([Window], [Window], [Window]))
getPanesInfo =
  getFocusedWin >>= \case
    Nothing -> pure Nothing
    Just focused -> do
      layout <- gets $ windowset >>> W.current >>> W.workspace >>> W.layout
      case List.splitOn "C2P " (show layout) of
        _ : s0 : _
          | _ <- ()
            , [(all', s1)] <- reads @[Word64] s0
            , [(left', s2)] <- reads @[Word64] s1
            , [(right', _s)] <- reads @[Word64] s2
            , let (focusedPane, unfocusedPane)
                    | focused `elem` left' = (left', right')
                    | otherwise = (right', left') ->
            pure $ Just (all', focusedPane, unfocusedPane)
        _ -> pure Nothing

getFocusedWin :: X (Maybe Window)
getFocusedWin =
  gets $ windowset >>> W.current >>> W.workspace >>> W.stack >>> fmap W.focus
