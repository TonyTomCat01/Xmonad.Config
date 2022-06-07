--
-- xmonad example config file for xmonad-0.9
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--
-- NOTE: Those updating from earlier xmonad versions, who use
-- EwmhDesktops, safeSpawn, WindowGo, or the simple-status-bar
-- setup functions (dzen, xmobar) probably need to change
-- xmonad.hs, please see the notes below, or the following
-- link for more details:
--
-- http://www.haskell.org/haskellwiki/Xmonad/Notable_changes_since_0.8
--

import Data.Monoid
import System.Exit
import XMonad

import qualified Data.Map as M
import qualified XMonad.StackSet as W

import XMonad.Hooks.ManageDocks (
    Direction2D (D, L, R, U),
    avoidStruts,
    docks,
    manageDocks,
 )
import XMonad.Layout.Gaps (
    Direction2D (D, L, R, U),
    GapMessage (DecGap, IncGap, ToggleGaps),
    gaps,
    setGaps,
 )

import XMonad.Layout.MagicFocus
import XMonad.Layout.Spacing

import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane

import XMonad.Actions.CycleWS

import XMonad.Hooks.EwmhDesktops

import XMonad.Hooks.ScreenCorners

import XMonad.Hooks.ShowWName

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal = "st"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Width of the window border in pixels.
--
myBorderWidth = 0

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod4Mask

-- NOTE: from 0.9.1 on numlock mask is set automatically. The numlockMask
-- setting should be removed from configs.
--
-- You can safely remove this even on earlier xmonad versions unless you
-- need to set it to something other than the default mod2Mask, (e.g. OSX).
--
-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
-- myNumlockMask   = mod2Mask -- deprecated in xmonad-0.9.1
------------------------------------------------------------

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces = ["Home \63451 : 1", "Code \63084 : 2", "Web \63044 : 3", "4", "5", "6", "7", "8", "9"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor = "#dddddd"
myFocusedBorderColor = "#ff0000"

------------------------------------------------------------------------

f = do
    -- spawn "~/.xmonad/bin/eww"
    sendMessage ToggleGaps

-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig{XMonad.modMask = modm}) =
    M.fromList $
        [ ((modm, xK_Return), spawn $ XMonad.terminal conf)
        , ((modm, xK_c), kill)
        , ((modm, xK_space), sendMessage NextLayout)
        ,
            ( (modm .|. controlMask, xK_space)
            , flashName
                def
                    { swn_font = "xft:FantasqueSansMono Nerd Font:style=Italic:size=20"
                    , swn_fade = 1
                    , swn_bgcolor = "#121212"
                    , swn_color = "#ffffff"
                    }
            )
        , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
        , -- ((modm .|. shiftMask, xK_space), sendMessage PreviousLayout)
          ((modm, xK_n), refresh)
        , ((modm, xK_Tab), windows W.focusDown)
        , ((modm, xK_j), windows W.focusDown)
        , ((modm, xK_k), windows W.focusUp)
        , ((modm, xK_m), windows W.focusMaster)
        , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)
        , ((modm .|. shiftMask, xK_j), windows W.swapDown)
        , ((modm .|. shiftMask, xK_k), windows W.swapUp)
        , ((modm, xK_h), sendMessage Shrink)
        , ((modm, xK_l), sendMessage Expand)
        , ((modm, xK_t), withFocused $ windows . W.sink)
        , ((modm, xK_comma), sendMessage (IncMasterN 1))
        , ((modm, xK_period), sendMessage (IncMasterN (-1)))
        , ((modm, xK_b), f)
        , ((modm .|. shiftMask, xK_q), io exitSuccess)
        , ((modm, xK_q), spawn "xmonad --recompile; xmonad --restart")
        ]
            ++ [ ((m .|. modm, k), windows $ f i)
               | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
               , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
               ]
            ++
            --
            -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
            -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
            --
            [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
            | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..]
            , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
            ]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig{XMonad.modMask = modm}) =
    M.fromList $
        -- mod-button1, Set the window to floating mode and move by dragging
        [
            ( (modm, button1)
            , ( \w ->
                    focus w >> mouseMoveWindow w
                        >> windows W.shiftMaster
              )
            )
        , -- mod-button2, Raise the window to the top of the stack
          ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
        , -- mod-button3, Set the window to floating mode and resize by dragging

            ( (modm, button3)
            , ( \w ->
                    focus w >> mouseResizeWindow w
                        >> windows W.shiftMaster
              )
            )
        ]

myLayout = screenCornerLayoutHook $ avoidStruts (tiled ||| Mirror tiled ||| Grid ||| TwoPane (3 / 100) (1 / 2) ||| ThreeCol 1 (3 / 100) (1 / 2) ||| ThreeColMid 1 (3 / 100) (1 / 2) ||| Full)
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100

myManageHook =
    composeAll
        [ resource =? "google-chrome" --> doF (W.shift "Web \63044 : 3")
        , resource =? "Conky" --> doIgnore
        ]

------------------------------------------------------------------------
-- Event handling

-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--

-- * NOTE: EwmhDesktops users should use the 'ewmh' function from

-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH event handling to your custom event hooks by
-- combining them with ewmhDesktopsEventHook.
--
myEventHook e = do
    screenCornerEventHook e

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
--

-- * NOTE: EwmhDesktops users should use the 'ewmh' function from

-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH logHook actions to your custom log hook by
-- combining it with ewmhDesktopsLogHook.
--
myLogHook =
    showWNameLogHook
        def
            { swn_font = "xft:FantasqueSansMono Nerd Font:style=Italic:size=20"
            , swn_fade = 1
            , swn_bgcolor = "#121212"
            , swn_color = "#ffffff"
            }

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.

-- * NOTE: EwmhDesktops users should use the 'ewmh' function from

-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add initialization of EWMH support to your custom startup
-- hook by combining it with ewmhDesktopsStartup.
--
myStartupHook :: X ()
myStartupHook = do
    addScreenCorners
        [ (SCUpperRight, nextWS)
        , (SCUpperLeft, prevWS)
        ]
    spawn "/$HOME/.xmonad/bin/autostart"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = xmonad $ ewmh defaults

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults =
    def -- simple stuff
        { terminal = myTerminal
        , focusFollowsMouse = myFocusFollowsMouse
        , borderWidth = myBorderWidth
        , modMask = myModMask
        , -- numlockMask deprecated in 0.9.1
          -- numlockMask        = myNumlockMask,
          workspaces = myWorkspaces
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , -- key bindings
          keys = myKeys
        , mouseBindings = myMouseBindings
        , -- hooks, layouts
          layoutHook = gaps [(L, 25), (R, 25), (U, 0), (D, 25)] $ spacingRaw False (Border 50 5 5 5) True (Border 5 5 5 5) True myLayout
        , manageHook = myManageHook
        , handleEventHook = myEventHook
        , logHook = myLogHook
        , startupHook = myStartupHook
        }
