#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++Imports y Variables++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Imports
from typing import List  # noqa: F401
from libqtile import bar, layout, widget
from libqtile.config import Click, Drag, Group, Key, Screen
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal
import os
import subprocess
from libqtile import hook
#Variables globales
mod = "mod4"
alt = "mod1"
terminal = "kitty"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++Keybindings++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
keys = [
    #--------------------------------------------------------
    #--------------WM Control--------------------------------
    #--------------------------------------------------------    
    # Switch between windows
    Key([mod], "h", lazy.layout.left(), lazy.window.bring_to_front(),
        desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), lazy.window.bring_to_front(),
        desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), lazy.group.prev_window(), lazy.window.bring_to_front(),
        desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), lazy.group.next_window(), lazy.window.bring_to_front(),
        desc="Move focus up"),
    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(),
        desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(),
        desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(),
        desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(),
        desc="Move window up"),
    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key([mod, "control"], "h", lazy.layout.grow_left(),
        desc="Grow window to the left"),
    Key([mod, "control"], "l", lazy.layout.grow_right(),
        desc="Grow window to the right"),
    Key([mod, "control"], "j", lazy.layout.grow_down(),
        desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(),
        desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(),
        desc="Reset all window sizes"),
    # Switch window focus to other pane(s) of stack
    Key([mod], "space", lazy.layout.next(),
        desc="Switch window focus to other pane(s) of stack"),
    # Floating
    Key([mod], "a", lazy.window.toggle_floating(),
        desc = "Toggle Window's Floating Mode"),
    # Fullscreen
    Key([mod], "s", lazy.window.toggle_fullscreen(),
        desc = "Toggle Fullscreen Mode"),
    # Swap panes of split stack
    Key([mod, "shift"], "space", lazy.layout.rotate(),
        desc="Swap panes of split stack"),
    # Toggle between layouts
    Key([mod], "Tab", lazy.next_layout(),
        desc="Toggle between layouts"),
    # Close a window    
    Key([mod], "w", lazy.window.kill(),
        desc="Kill focused window"),
    # Restart wm
    Key([mod, "control"], "r", lazy.restart(),
        desc="Restart qtile"),
    # Cerrar sesión de qtile 
    Key([mod, "control"], "q", lazy.shutdown(),
        desc="Shutdown qtile"),
    # Moverse entre grupos
    Key([mod], "Left", lazy.screen.prev_group(),
        desc="move to the previous group"),
    Key([mod], "Right", lazy.screen.next_group(),
        desc="move to the next group"),
    Key([mod], "b", lazy.screen.toggle_group(),
        desc="move to the last visited group"),
    # Mover a otra pantalla
    Key([mod, alt], "9", lazy.to_screen(0), lazy.group.toscreen(0),
        desc="move to screen 1"),
    Key([mod, alt], "0", lazy.to_screen(1), lazy.group.toscreen(1),
        desc="move to screen 2"),
    #

    
    #--------------------------------------------------------
    #--------------Controles Multimedia----------------------
    #--------------------------------------------------------
    # Brightness
    # Necesario instalar xorg-xbacklight 
    Key([], "XF86MonBrightnessUp", lazy.spawn("xbacklight -inc 5"),
        desc="Increase Brightness"),
    Key([], "XF86MonBrightnessDown", lazy.spawn("xbacklight -dec 5"),
        desc="Decrease Brightness"), 
    # Sound
    Key([], "XF86AudioMute", lazy.spawn("amixer set Master toggle"),
        desc="Mute/Unmute"),
    Key([], "XF86AudioLowerVolume", lazy.spawn("amixer set Master 5%-"),
        desc="LowerVolume"),
    Key([], "XF86AudioRaiseVolume", lazy.spawn("amixer set Master 5%+"),
        desc="RaiseVolume"),
    Key([mod], "m", lazy.spawn("pavucontrol"),
        desc="Audio Control"),

    #--------------------------------------------------------
    #--------------Launch Apps-------------------------------
    #--------------------------------------------------------
    # rofi menu
    Key([mod], "x", lazy.spawn("rofi -combi-modi window,drun,ssh -icons-theme 'Newaita' -show combi"),
        desc="Launch rofi-menu"),
    # dmenu
    Key([mod], "z", lazy.spawn("dmenu_run"),
        desc="Launch dmenu"),
    # terminal
    Key([mod], "Return", lazy.spawn(terminal),
        desc="Launch terminal")
]


groups = [Group(i) for i in "1234567890"]
for i in groups:
    keys.extend([
        # mod1 + letter of group = switch to group
        Key([mod], i.name, lazy.group[i.name].toscreen(),
            desc="Switch to group {}".format(i.name)),

        # mod1 + shift + letter of group = switch to & move focused window to group
        Key([mod, "shift"], i.name, lazy.window.togroup(i.name, switch_group=True),
            desc="Switch to & move focused window to group {}".format(i.name)),
        # Or, use below if you prefer not to switch to that group.
        # # mod1 + shift + letter of group = move focused window to group
        # Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
        #     desc="move focused window to group {}".format(i.name)),
    ])

layouts = [
    # layout.Max(),
    layout.Bsp(),
    layout.Stack(num_stacks=2),
    # Try more layouts by unleashing below layouts.
    layout.Columns(),
    layout.Matrix(),
    layout.MonadTall(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    layout.Zoomy(),
]

widget_defaults = dict(
    font='Source Code Pro',
    fontsize=12,
    padding=3,
)

extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        bottom=bar.Bar(
            [
                widget.Image(
                    filename = '~/Imágenes/icon.png'),
                widget.GroupBox(),
                widget.Sep(
                    foreground = 'a4e282',
                    linewidth = 2,),
                # widget.Prompt(),
                widget.WindowName(),
                widget.Chord(
                    chords_colors={'launch': ("#ff0000", "#ffffff"),},
                    name_transform=lambda name: name.upper(),),
                # widget.TextBox("default config", name="default"),
                # widget.TextBox("Press &lt;M-r&gt; to spawn", foreground="#d75f5f"),
                # widget.DF(),
                #widget.Mpris2(
                #    objname = 'org.mpris.MediaPlayer2.spotify',
                #    name = "spotify"),
                widget.Pomodoro(
                    color_inactive = 'dc7e61'),
                widget.Sep(
                    foreground = 'a4e282',
                    linewidth = 2,),
                widget.NetGraph(),
                #widget.CapsNumLockIndicator(),
                widget.Sep(
                    foreground = 'a4e282',
                    linewidth = 2,),
                # widget.CheckUpdates(),
                widget.Systray(),
                # Necesario instalar brightnessctl y conocer el dispositivo de backlight con
                #   ls /sys/class/backlight/
                #widget.Sep(
                #    foreground = 'a4e282',
                #    linewidth = 2,),
                #widget.Backlight(
                #    change_command = 'brightnessctl set 60%',
                #    backlight_name = 'intel_backlight',
                #    step = 5,
                #    format = ' {percent:2.0%}',),
                #widget.Sep(
                #    foreground = 'a4e282',
                #    linewidth = 2,),
                #widget.Battery(
                #    format = '{char}{percent:2.0%} {hour:d}:{min:02d}',
                #    update_interval = 10,
                #    low_foreground = 'FF0000',),
                widget.Sep(
                    foreground = 'a4e282',
                    linewidth = 2,),
                widget.Clock(format='%d-%b %a %I:%M'),
                widget.Sep(
                    foreground = 'a4e282',
                    linewidth = 2,),
                widget.CurrentLayout(),
                widget.Sep(
                    foreground = 'a4e282',
                    linewidth = 2,),
                widget.QuickExit(),
            ],
            28
        ),
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
main = None  # WARNING: this is deprecated and will be removed soon
follow_mouse_focus = True
bring_front_click = True
cursor_warp = False
floating_layout = layout.Floating(float_rules=[
    # Run the utility of `xprop` to see the wm class and name of an X client.
    {'wmclass': 'confirm'},
    {'wmclass': 'dialog'},
    {'wmclass': 'download'},
    {'wmclass': 'error'},
    {'wmclass': 'file_progress'},
    {'wmclass': 'notification'},
    {'wmclass': 'splash'},
    {'wmclass': 'toolbar'},
    {'wmclass': 'confirmreset'},  # gitk
    {'wmclass': 'makebranch'},  # gitk
    {'wmclass': 'maketag'},  # gitk
    {'wname': 'branchdialog'},  # gitk
    {'wname': 'pinentry'},  # GPG key password entry
    {'wmclass': 'ssh-askpass'},  # ssh-askpass
    {'wmclass': 'kitty'},
    {'wmclass': 'zoom'},
    {'wmclass': 'pavucontrol'},
])
auto_fullscreen = False
focus_on_window_activation = "smart"

# AutoStart
@hook.subscribe.startup_once
def autostart():
    home = os.path.expanduser('~')
    subprocess.Popen(["xfce4-power-manager"])
    subprocess.Popen(["light-locker", "--lock-after-screensaver=20",  "--lock-on-lid"])
    subprocess.Popen(["nitrogen", "--restore"])
    subprocess.Popen(["dropbox"])
    subprocess.Popen(["pa-applet"])
    subprocess.Popen(["nm-applet"])

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"

