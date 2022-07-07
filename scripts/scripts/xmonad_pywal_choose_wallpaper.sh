#!/usr/bin/env bash
# Dentro del selector usar x para elegir una imágen
# Luego con q salir, la imágen seleccionada se le pasa a pywal y al lockscreen

input="$HOME"/wallpapers

selected=$(sxiv -N "change_wallpaper_pywal" -ot $input)
if [[ "$selected" != "" ]]
then
    wal --backend colorthief -i "$selected"

    # killall xmobar
    # "$HOME"/.xmonad/scripts/xmobar_pywal_color_sync.sh
    xmonad --recompile
    xmonad --restart

    killall -q dunst
    sh "$HOME"/scripts/dunst_xr_theme_changer.sh
    /usr/bin/dunst -conf "$HOME"/.config/dunst/dunstrc_xr_colors & disown

    betterlockscreen -u "$selected"
fi
