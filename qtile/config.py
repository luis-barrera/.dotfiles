#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++ Imports y Variables +++++++++++++++++++++++++++++++
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
#+++++++++++++++++++++++ Keybindings +++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
keys = [
    #--------------------------------------------------------
    #----------- Controles de Generales ---------------------
    #--------------------------------------------------------    
    # Cambiar entre ventanas
    Key([mod], "h",
        lazy.layout.left(),
        #lazy.window.bring_to_front(),
        desc="Move focus to left"),
    Key([mod], "l",
        lazy.layout.right(),
        #lazy.window.bring_to_front(),
        desc="Move focus to right"),
    Key([mod], "j",
        #lazy.layout.down(),
        lazy.group.prev_window(),
        #lazy.window.bring_to_front(),
        desc="Move focus down"),
    Key([mod], "k",
        #lazy.layout.up(),
        lazy.group.next_window(),
        #lazy.window.bring_to_front(),
        desc="Move focus up"),
    # Mover ventanas a otros lugares del layout, funciona con solo algunos layouts
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(),
        desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(),
        desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(),
        desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(),
        desc="Move window up"),
    # Otros keybindings, para otros tipos de layouts 
    Key([mod, "shift"], "space",
        lazy.layout.rotate(),
        lazy.layout.flip()), # xmonad-tall
    Key([mod, "shift"], "k", lazy.layout.shuffle_up()), # Stack, xmonad-tall
    Key([mod, "shift"], "j", lazy.layout.shuffle_down()), # Stack, xmonad-tall
    Key([mod, "control"], "l",
        lazy.layout.add(),                # Stack
        lazy.layout.increase_ratio(),     # Tile
        lazy.layout.maximize()),           # xmonad-tall
    Key([mod, "control"], "h",
        lazy.layout.delete(),             # Stack
        lazy.layout.decrease_ratio(),     # Tile
        lazy.layout.normalize()),          # xmonad-tall
    Key([mod, "control"], "k",
        lazy.layout.shrink(),             # xmonad-tall
        lazy.layout.decrease_nmaster()),   # Tile
    Key([mod, "control"], "j",
        lazy.layout.grow(),               # xmonad-tall
        lazy.layout.increase_nmaster()),   # Tile
    # Cambiar tamaño de las ventanas en alguno layouts que lo permiten 
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
    
    #--------------------------------------------------------
    #------------- Controles Multimedia ---------------------
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
    #-------------- Launch Apps -----------------------------
    #--------------------------------------------------------
    # rofi menu
    Key([mod], "x", lazy.spawn("rofi -combi-modi window,drun,ssh -icons-theme 'Newaita' -show combi"),
        desc="Launch rofi-menu"),
    # dmenu
    Key([mod], "z", lazy.spawn("dmenu_run"),
        desc="Launch dmenu"),
    # terminal
    Key([mod], "Return", lazy.spawn(terminal),
        desc="Launch terminal"),
    # Firefox
    Key([mod], "c", lazy.spawn("firefox"),
        desc="Launch Firefox"),
]

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++ Mouse Bindings +++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++ Grupos ++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#groups = [Group(i) for i in ["1", "2", "3", "4", "5", "6", "7", "8"]]
groups = [Group("1", spawn="", layout="bsp"),
    Group("2", spawn=["typora", "pomotroid"], layout="floating"),
    Group("3", spawn="", layout="bsp"),
    Group("4", spawn="", layout="bsp"),
    Group("5", spawn="", layout="bsp"),
    Group("6", spawn="", layout="bsp"),
    Group("7", spawn="", layout="bsp"),
    Group("8", spawn="", layout="bsp"),
#    Group("9", spawn="", layout="bsp")
    ]

for i in groups:
    keys.extend([
        # mod1 + letter of group = switch to group
        Key([mod], i.name, lazy.group[i.name].toscreen(),
            desc="Switch to group {}".format(i.name)),
        # switch to & move focused window to group
        Key([mod, "shift"], i.name, lazy.window.togroup(i.name, switch_group=True),
            desc="Switch to & move focused window to group {}".format(i.name)),
        # # mod1 + shift + letter of group = move focused window to group
        # Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
        #     desc="move focused window to group {}".format(i.name)),
    ])

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++ Layouts +++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Colores 
border = dict(
    border_focus='#f6832f',
    border_normal='#b8b5db',
    border_width=2,
    margin = 5)

layouts = [
    layout.Bsp(**border),
    layout.Stack(num_stacks=1, **border),
    layout.Columns(**border),
    layout.Matrix(**border),
    layout.MonadTall(**border),
    layout.Zoomy(**border),
    layout.Tile(**border),
    layout.Floating(**border),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Max(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
]

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++ Bar y Widgets ++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Widgets defaults
widget_defaults = dict(
    font='Source Code Pro',
    fontsize=12,
    padding=3,
)
extension_defaults = widget_defaults.copy()
sep_defs = dict(foreground = 'bee12a', linewidth = 2)

# Bar
screens = [
    Screen(
        bottom=bar.Bar(
            [
                widget.Image(
                    filename = '~/Imágenes/icon.png'),
                widget.GroupBox(
                    highlight_method='line',
                    #highlight_color=['000000', 'dd8a6f'],
                    this_current_screen_border='f64a32'),
                widget.Sep(**sep_defs),
                widget.TaskList(),
                widget.Chord(
                    chords_colors={'launch': ("#ff0000", "#ffffff"),},
                    name_transform=lambda name: name.upper(),),
                widget.Mpris2(name='spotify',
                              stop_pause_text='',
                              scroll_chars=None,
                              display_metadata=['xesam:title', 'xesam:artist'],
                              objname="org.mpris.MediaPlayer2.spotify"),
                widget.Sep(**sep_defs),
                widget.Systray(),
                widget.Sep(**sep_defs),
                widget.Clock(format='%d-%b %a %I:%M'),
                widget.Sep(**sep_defs),
                widget.CurrentLayout(), 
            ],
            28, opacity=0.8
        ),
    ),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
main = None  # WARNING: this is deprecated and will be removed soon
follow_mouse_focus = False
bring_front_click = True
cursor_warp = False

# Floating Layout
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
    # {'wmclass': 'kitty'},
    {'wmclass': 'zoom'},
    {'wmclass': 'keepassxc'},
    {'wmclass': 'pavucontrol'},
], **border)
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
    
#@hook.subscribe.client_new
#def modify_window(client):
#    if (client.window.get_wm_transient_for() or client.window.get_wm_type() in floating_types):
#        client.floating = True

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
