#! /bin/bash

text=$1

layout=$(echo "$text" | awk --field-separator=": " '{print $2}' | cut -f 3- -d ' ')

dunstify -D 192
dunstify -r 192 -a "Xmonad Layout" -u normal "Cambiado a Layout: $layout" -t 2000
