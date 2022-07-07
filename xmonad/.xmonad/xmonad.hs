-- TODO: nots  dunstify -h int:value:12 a
--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import Data.Monoid
import Data.Ratio ((%))
import System.Exit

import XMonad.Prompt
import XMonad.Prompt.OrgMode (orgPrompt)
import XMonad.Prompt.Layout
import XMonad.Prompt.Workspace
import XMonad.Prompt.Shell
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Ungrab

import XMonad.Layout.Accordion
import XMonad.Layout.Tabbed
import XMonad.Layout.SimpleFloat
import XMonad.Layout.NoBorders
import XMonad.Layout.BorderResize
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Gaps
import XMonad.Layout.AvoidFloats
import XMonad.Layout.BoringWindows
import XMonad.Layout.Minimize
import XMonad.Layout.Maximize
import XMonad.Layout.Dishes
import XMonad.Layout.PerWorkspace

import XMonad.Actions.FloatKeys
import XMonad.Actions.Minimize

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Util.EZConfig (additionalKeys)
import Graphics.X11.ExtraTypes.XF86

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run
import XMonad.Util.SpawnOnce (spawnOnce)

import Data.Bifunctor (bimap)
import Data.Char as DC
import Data.List as DL
import Data.Maybe (catMaybes, fromMaybe)
import System.IO.Unsafe (unsafePerformIO)
import XMonad.Core (installSignalHandlers)

import XMonad.Actions.GroupNavigation
import XMonad.Actions.CycleWS

------------------------------------------------------------------------
-- Obtener datos de xresources
splitAtColon :: String -> Maybe (String, String)
splitAtColon str = splitAtTrimming str <$> DL.elemIndex ':' str
  where
    splitAtTrimming :: String -> Int -> (String, String)
    splitAtTrimming s idx = bimap trim (trim . tail) $ splitAt idx s
    trim :: String -> String
    trim = DL.dropWhileEnd DC.isSpace . DL.dropWhile DC.isSpace

getFromXres :: String -> IO String
getFromXres key = do
  installSignalHandlers
  fromMaybe "" . findValue key <$> runProcessWithInput "xrdb" ["-query"] ""
  where
    findValue :: String -> String -> Maybe String
    findValue xresKey xres =
      snd <$>
      DL.find ((== xresKey) . fst) (catMaybes $ splitAtColon <$> lines xres)

xProp :: String -> String
xProp = unsafePerformIO . getFromXres

-- Colores de xresources
xColorFg :: String
xColorFg = xProp "*.foreground"

xColorBg :: String
xColorBg = xProp "*.background"

xColor :: String -> String
xColor a = xProp $ "*.color" ++ a
------------------------------------------------------------------------
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "kitty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

(~?) :: (Eq a, Functor m) => m [a] -> [a] -> m Bool
q ~? x = fmap (x `isInfixOf`) q


-- Width of the window border in pixels.
--
myBorderWidth   = 2

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
-- myModMask       = mod1Mask
myModMask       = mod4Mask -- Super

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
-- myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]
myWorkspaces = [(xK_a, "a"),
                (xK_s, "s"),
                (xK_d, "d"),
                (xK_f, "f"),
                (xK_q, "q"),
                (xK_w, "w"),
                (xK_e, "e"),
                (xK_1, "1"),
                (xK_2, "2"),
                (xK_3, "3"),
                (xK_4, "4"),
                (xK_5, "5")]
myWorkspacesNames = [ ws | (key, ws) <- myWorkspaces ] -- Mi primer config propia xd

-- Border colors for unfocused and focused windows, respectively.
--
-- myNormalBorderColor  = "#dddddd"
-- myFocusedBorderColor = "#ff0000"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launch a terminal
    -- [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    [ ((modm,               xK_Return), spawn $ XMonad.terminal conf)

    , ((modm .|. shiftMask,               xK_c     ), spawn "nemo")

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "dmenu_run")

    -- launch gmrun
    -- , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")
    , ((modm, xK_z                   ), spawn "rofi -show combi")

    -- close focused window
    , ((modm .|. shiftMask, xK_x     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    -- , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)
    -- , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    -- , ((modm .|. shiftMask, xK_t     ), withFocused $ keysAbsResizeWindow ((-100), (-100)) (0, 0) >> keysMoveWindowTo (800, 450) (1%2, 1%2))
    , ((modm .|. shiftMask, xK_t     ), withFocused $ keysResizeWindow ((-400), (-200)) (1%2, 1%2))
          -- keysResizeWindow (dx, dy) (gx2, gy2) windowId)
    -- , ((modm .|. shiftMask,               xK_t     ),
    --     withFocused $ windows . W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2))

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    , ((modm              , xK_Delete), spawn "loginctl lock-session")


    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    -- , ((modm .|. shiftMask, xK_r     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    -- , ((modm              , xK_r     ), spawn "xmonad --recompile; xmonad --restart")
    , ((modm .|. shiftMask, xK_r     ), spawn "xmonad --recompile; xmonad --restart")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))

    , ((0, xF86XK_PowerDown),            spawn "systemctl suspend")
    , ((0, xF86XK_AudioRaiseVolume),     spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%; sh scripts/volume-not.sh")
    , ((0, xF86XK_AudioLowerVolume),     spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%; sh scripts/volume-not.sh")
    , ((modm .|. controlMask, xK_plus),  spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%; sh scripts/volume-not.sh")
    , ((modm .|. controlMask, xK_minus), spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%; sh scripts/volume-not.sh")
    , ((0, xF86XK_AudioMute),            spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle; sh scripts/volume-not.sh")
    , ((0, xF86XK_AudioMute),            spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle; sh scripts/volume-not.sh")
    , ((0, xF86XK_MonBrightnessUp),      spawn "xbacklight -inc 5")
    , ((0, xF86XK_MonBrightnessDown),    spawn "xbacklight -dec 10")

    , ((modm, xK_Escape), nextMatch History (return True))
    , ((mod1Mask ,               xK_Escape),     toggleWS)

    , ((modm, xK_y), spawn "sh /home/luisbarrera/.config/polybar/hide.sh")

    , ((modm,               xK_n     ), withFocused minimizeWindow)
    , ((modm .|. shiftMask, xK_n     ), withLastMinimized maximizeWindowAndFocus)
    , ((modm, xK_m), withFocused (sendMessage . maximizeRestore))

    , ((modm, xK_b), orgPrompt myXPConfig "TODO" "/home/luisbarrera/google-drive/org-mode/draft.org")
    -- , ((modm, xK_b     ), layoutPrompt myXPConfig)
    -- , ((modm .|. shiftMask, xK_m     ), workspacePrompt def (windows . W.shift))
    -- , ((modm .|. controlMask, xK_x), shellPrompt myXPConfig)
    , ((modm, xK_plus ), unGrab >> spawn "maim | tee ~/screenshots/$(date +%s).png | xclip -selection clipboard -t image/png")
    , ((modm .|. shiftMask, xK_plus ), unGrab >> spawn "maim -s | tee ~/screenshots/$(date +%s).png | xclip -selection clipboard -t image/png")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    -- [((m .|. modm, k), windows $ f i)
    --     | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    --     , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    [((myModMask, key), windows $ W.greedyView ws)
        | (key, ws) <- myWorkspaces
    ]
    ++
    [((myModMask .|. shiftMask, key), windows $ W.shift ws)
        | (key, ws) <- myWorkspaces
    ]
    -- ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    -- [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = maximizeWithPadding 0 (avoidFloats $
  minimize $
  smartBorders $
  boringWindows $
  avoidStruts (gaps gapdims tiled
                -- ||| Mirror tiled
                -- ||| Accordion
                ||| Dishes 2 (1/6)
                ||| gaps gapdims emptyBSP
                ||| simpleFloat
                ||| simpleTabbed
                ||| noBorders Full))
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 5/8
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
     --
     gapdims = [(U,gap), (R,gap), (D,gap), (L,gap)]
     gap = 10

myXPConfig = def
    {
      font = "xft:JetBrainsMono Nerd Font:size=12:bold:antialias=true"
    -- , bgColor = "#200000"
    -- , fgColor = "#CFCFCF"
    -- , height = 22
    -- , position = CenteredAt 0.5 0.5
    -- , promptBorderWidth = 2
    , showCompletionOnTab = True
    }
------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
-- BUG
myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Firefox"        --> doF (W.shift "e")
    , className =? "firefox"        --> hasBorder False
    , title     =? "btop"           --> doShift "5"
    -- , className ~? "join"           --> doRectFloat (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2))
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore]
newManageHook = myManageHook <> manageHook def

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty
-- myEventHook = ewmhDesktopsEventHook

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
-- myLogHook = return ()
myLogHook = historyHook

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
          -- spawnOnce "wal -i /home/luisbarrera/wallpapers"
          spawn "sh /home/luisbarrera/.config/polybar/launch.sh"
          -- spawn "wal --backend colorthief -i /home/luisbarrera/wallpapers"
          spawn "wal -R; cat /home/luisbarrera/.cache/wal/wal | xargs feh --bg-fill $1 | xrdb -merge /home/luisbarrera/.Xresources"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
  -- xmproc <- spawnPipe "xmobar -x 0 /home/luisbarrera/.xmonad/xmobarrc"
  -- xmproc <- spawnPipe "sh /home/luisbarrera/.config/polybar/hide.sh"
  xmonad $ ewmhFullscreen . ewmh $ docks defaults

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspacesNames,
        -- normalBorderColor  = myNormalBorderColor,
        normalBorderColor  = xColorBg,
        -- focusedBorderColor = myFocusedBorderColor,
        focusedBorderColor = xColor "3",

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = newManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
