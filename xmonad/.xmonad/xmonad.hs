-------------------------------------------------------------------------------
-- ModulesModules
-- Xmonad
import XMonad
import XMonad.Core (installSignalHandlers)
import qualified XMonad.StackSet as W

-- Datatype
import qualified Data.Map        as M
import Data.Monoid
import Data.Ratio ((%))
import Data.Bifunctor (bimap)
import Data.Char as DC
import Data.List as DL
import Data.Maybe (catMaybes, fromMaybe)

-- System
import System.Exit
import System.IO.Unsafe (unsafePerformIO)

-- Prompts
import XMonad.Prompt
import XMonad.Prompt.OrgMode (orgPrompt)
import XMonad.Prompt.Layout
import XMonad.Prompt.Workspace
import XMonad.Prompt.Shell

-- Hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog

-- Layouts
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
import XMonad.Layout.MagicFocus

-- Extra actions
import XMonad.Actions.FloatKeys
import XMonad.Actions.Minimize
import XMonad.Actions.SpawnOn
import XMonad.Actions.GroupNavigation
import XMonad.Actions.CycleWS
import qualified XMonad.Actions.FlexibleManipulate as Flex
import XMonad.Actions.FloatSnap
import XMonad.Actions.Promote

-- Extra utilities
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Ungrab
import XMonad.Util.Run
import XMonad.Util.SpawnOnce (spawnOnce)

-- Extra keys of keyboard
import Graphics.X11.ExtraTypes.XF86


------------------------------------------------------------------------
-- Customs functions definition
-- Send to a script actual layout to print a notification
layoutNot m = spawn $ "sh /home/luisbarrera/scripts/xmonad_layout_not.sh " ++ show m

-- regex operator for WM_name in hooks
-- (~?) :: (Eq a, Functor m) => m [a] -> [a] -> m Bool
-- q ~? x = fmap (x `isInfixOf`) q


------------------------------------------------------------------------
-- Get data from Xresources file
-- Parse output from "xrdb --query" command
splitAtColon :: String -> Maybe (String, String)
splitAtColon str = splitAtTrimming str <$> DL.elemIndex ':' str
  where
    splitAtTrimming :: String -> Int -> (String, String)
    splitAtTrimming s idx = bimap trim (trim . tail) $ splitAt idx s
    trim :: String -> String
    trim = DL.dropWhileEnd DC.isSpace . DL.dropWhile DC.isSpace

-- Call command "xrdb --query" and parse
getFromXres :: String -> IO String
getFromXres key = do
  installSignalHandlers
  fromMaybe "" . findValue key <$> runProcessWithInput "xrdb" ["-query"] ""
  where
    findValue :: String -> String -> Maybe String
    findValue xresKey xres =
      snd <$>
      DL.find ((== xresKey) . fst) (catMaybes $ splitAtColon <$> lines xres)

-- Get parsed data
xProp :: String -> String
xProp = unsafePerformIO . getFromXres

-- Foreground color
xColorFg :: String
xColorFg = xProp "*.foreground"
-- Background color
xColorBg :: String
xColorBg = xProp "*.background"
-- Other color from 0 to 15
xColor :: String -> String
xColor a = xProp $ "*.color" ++ a


------------------------------------------------------------------------
-- Custom variable
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
myTerminal = "kitty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
myBorderWidth = 2

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
myModMask = mod4Mask -- Super
-- myModMask = mod1Mask -- Alt Izq

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
-- myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]
myWorkspaces = [(xK_a, "a"), -- (keyboard key, workspace asigned)
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
                (xK_5, "5"),
                (xK_0, "0")]  -- Workspace para segundo monitor ]
-- List of only workspaces names
myWorkspacesNames = [ ws | (key, ws) <- myWorkspaces ] -- Mi primer config propia xd

toggleFloat w = windows (\s -> if M.member w (W.floating s)
                            then W.sink w s
                            else (W.float w (W.RationalRect (1/10) (1/10) (8/10) (8/10)) s))


------------------------------------------------------------------------
-- Key bindings
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- Launch a terminal
    [ ((modm, xK_Return), spawn $ XMonad.terminal conf)

    -- Launch file explorer
    , ((modm .|. shiftMask, xK_u), spawn "nemo")
    -- Launch gui editor
    , ((modm .|. shiftMask, xK_i), spawn "neovide")
    -- Launch browser
    , ((modm .|. shiftMask, xK_o), do spawnOn "e" "firefox"; windows $ W.greedyView "e")

    -- Launch dmenu
    , ((modm, xK_p), spawn "dmenu_run")

    -- Launch launcher
    , ((modm, xK_z), spawn "rofi -show combi")

    -- Close focused window
    , ((modm .|. shiftMask, xK_x), kill)

    -- Rotate through the available layout algorithms and send a notification
    , ((modm, xK_space), sendMessage NextLayout >> dynamicLogString def >>= layoutNot)
    -- , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space), (setLayout $ XMonad.layoutHook conf) >> dynamicLogString def >>= layoutNot)

    -- Resize viewed windows to the correct size
    -- , ((modm, xK_n), refresh)

    -- Move focus to the next window
    , ((modm, xK_Tab), windows W.focusDown)

    -- Move focus to the next window
    , ((modm, xK_j), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm, xK_k), windows W.focusUp)

    -- Move focus to the master window
    , ((modm, xK_m), windows W.focusMaster)

    -- Raise to top
    , ((modm, xK_ntilde), windows W.shiftMaster)

    -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j), windows W.swapDown)

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k), windows W.swapUp)

    -- Shrink the master area
    , ((modm, xK_h), sendMessage Shrink)

    -- Expand the master area
    , ((modm, xK_l), sendMessage Expand)

    -- Push window back into tilling
    -- , ((modm, xK_t), withFocused $ windows . W.sink)
    -- Set window in floating and reduce size
    -- , ((modm .|. shiftMask, xK_t), withFocused (keysMoveWindowTo (512,384) (1%2,1%2)))
    -- Toggle float or tilling
    , ((modm, xK_t), withFocused toggleFloat)

    -- Minimie window
    , ((modm,               xK_n     ), withFocused minimizeWindow)
    -- unMinimize last minimized window
    , ((modm .|. shiftMask, xK_n     ), withLastMinimized maximizeWindowAndFocus)
    -- Toggle Maxime of window
    , ((modm, xK_m), withFocused (sendMessage . maximizeRestore))

    -- Increment the number of windows in the master area
    , ((modm, xK_comma), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm, xK_period), sendMessage (IncMasterN (-1)))

    -- Focus to last focused window
    , ((modm, xK_Escape), nextMatch History (return True))
    -- Focus last visited window
    , ((mod1Mask, xK_Escape), toggleWS)

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    -- , ((modm, xK_b), sendMessage ToggleStruts)

    -- Quit xmonad
    -- , ((modm .|. shiftMask, xK_r), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm .|. shiftMask, xK_r), spawn "xmonad --recompile; xmonad --restart")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_apostrophe), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ] ++

    -- Media keys
    -- Mute mic
    [ ((0, xF86XK_AudioMicMute), spawn "sh scripts/mic-not.sh")
    -- Raise Volume
    -- , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%; sh scripts/volume-not.sh")
    , ((modm .|. controlMask, xK_plus), spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%; sh scripts/volume-not.sh")
    -- Lower Volume
    -- , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%; sh scripts/volume-not.sh")
    , ((modm .|. controlMask, xK_minus), spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%; sh scripts/volume-not.sh")
    -- Mute Sound
    , ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle; sh scripts/volume-not.sh")
    -- Previous song
    , ((modm .|. controlMask, xK_braceleft), spawn "playerctl --player=playerctld previous")
    -- Next song
    , ((modm .|. controlMask, xK_braceright), spawn "playerctl --player=playerctld next")
    -- Pause song
    , ((modm .|. controlMask, xK_space ), spawn "playerctl --player=playerctld play-pause")

    -- Lock screen
    , ((modm, xK_Delete), spawn "loginctl lock-session")
    , ((0, xF86XK_PowerDown), spawn "loginctl lock-session")

    -- Increase brightness
    , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 5")
    -- Decrease brightness
    , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10")

    -- Toogle polybar
    , ((modm, xK_y), spawn "sh /home/luisbarrera/.config/polybar/hide.sh")

    -- Prompt to add a TODO-Note in an org file
    , ((modm, xK_b), orgPrompt myXPConfig "TODO" "/home/luisbarrera/google-drive/org-mode/draft.org")

    -- Take an screenshot an save it to dir and clipboard
    , ((modm, xK_plus ), unGrab >> spawn "maim | tee ~/screenshots/$(date +%s).png | xclip -selection clipboard -t image/png")
    -- Ask user for area to take screenshot of and save it to dir and clipboard
    , ((modm .|. shiftMask, xK_plus ), unGrab >> spawn "maim -s | tee ~/screenshots/$(date +%s).png | xclip -selection clipboard -t image/png")
    ]
    ++


    -- Switch to workspace
    [((myModMask, key), windows $ W.greedyView ws)
        | (key, ws) <- myWorkspaces
    ]
    ++
    -- Send focused window to workspace
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
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
--
myDeco = def {activeColor = xColor "1"
                  , inactiveColor = xColorBg
                  , urgentColor = xColor "3"
                  , activeBorderColor = xColorFg
                  , inactiveBorderColor = xColorBg
                  , urgentBorderColor = xColor "3"
                  , activeBorderWidth = 2
                  , inactiveBorderWidth = 2
                  , urgentBorderWidth = 2
                  , activeTextColor = xColorFg
                  , inactiveTextColor = xColorFg
                  , urgentTextColor = xColor "3"
                  , fontName = "xft:JetBrainsMono Nerd Font:size=8:antialias=true"
                  -- , decoWidth = 200
                  , decoHeight = 20
                  }


-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
myLayout = maximizeWithPadding 0 ( minimize $
  -- magicFocus $
  smartBorders $
  boringWindows $
  avoidStruts (gaps gapdims tiled
                -- ||| Mirror tiled
                -- ||| Accordion
                ||| Dishes 2 (1/6)
                ||| gaps gapdims emptyBSP
                ||| simpleFloat' shrinkText myDeco
                ||| tabbed shrinkText myDeco
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
     -- Dims for gaps
     gapdims = [(U,gap), (R,gap), (D,gap), (L,gap)]
     gap = 10


------------------------------------------------------------------------
-- Prompts config
myXPConfig = def
    { font = "xft:JetBrainsMono Nerd Font:size=12:bold:antialias=true"
    , bgColor = xColorBg
    , fgColor = xColorFg
    , position = CenteredAt 0.5 0.8
    , height = 60
    , promptBorderWidth = 1
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

bigRationalRectFloatHook = W.RationalRect (1/8) (1/8) (6/8) (6/8)
smallRationalRectFloatHook = W.RationalRect (2/8) (1/8) (4/8) (6/8)
myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , title     =? "btop"           --> doF (W.shift "5")
    , className =? "firefox"        --> doF (W.shift "e")
    , className =? "firefox"        --> hasBorder False
    , className =? "Emacs"          --> hasBorder False
    , className =? "Emacs"          --> doF (W.shift "q")
    , className =? "Spotify"        --> doF (W.shift "f")
    , className =? "mpv"            --> doRectFloat smallRationalRectFloatHook
    , className =? "KeePassXC"      --> doRectFloat bigRationalRectFloatHook
    -- Zoom
    , className =? "zoom "          --> doRectFloat bigRationalRectFloatHook
    , className ^? "join"           --> doRectFloat smallRationalRectFloatHook
    --- Desktops
    , className =? "Xdg-desktop-portal-gtk"  --> doRectFloat smallRationalRectFloatHook
    , className =? "Dunst"          --> doIgnore
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    ]
newManageHook = myManageHook <> manageHook def


------------------------------------------------------------------------
-- Bring clicked floating window to the front
floatClickFocusHandler :: Event -> X All
floatClickFocusHandler ButtonEvent { ev_window = w } = do
	withWindowSet $ \s -> do
		if isFloat w s
		   then (focus w >> promote)
		   else return ()
		return (All True)
		where isFloat w ss = M.member w $ W.floating ss
floatClickFocusHandler _ = return (All True)


------------------------------------------------------------------------
-- Event handling
-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
-- myEventHook = mempty
-- myEventHook = ewmhDesktopsEventHook
myEventHook = floatClickFocusHandler


------------------------------------------------------------------------
-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
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
          spawn "sh /home/luisbarrera/.config/polybar/launch.sh"
          -- spawn "wal -R; cat /home/luisbarrera/.cache/wal/wal | xargs feh --bg-fill $1 | xrdb -merge /home/luisbarrera/.Xresources"
          -- spawn "killall -q dunst"
          -- spawn "sh /home/luisbarrera/scripts/dunst_xr_theme_changer.sh"
          -- spawn "/usr/bin/dunst -conf /home/luisbarrera/.config/dunst/dunstrc_xr_colors & disown"
          -- spawn "wal --backend colorthief -i /home/luisbarrera/wallpapers"
          -- spawnOnce "wal -i /home/luisbarrera/wallpapers"


------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
-- Run xmonad with the settings you specify. No need to modify this.
main = do
  xmonad $ ewmhFullscreen . ewmh $ docks defaults

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
defaults = def {
        -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspacesNames,
        normalBorderColor  = xColorBg,
        focusedBorderColor = xColor "3",

        -- hooks, layouts
        manageHook         = newManageHook,
        layoutHook         = myLayout,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook,

        -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings
    }

-- TODO
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
