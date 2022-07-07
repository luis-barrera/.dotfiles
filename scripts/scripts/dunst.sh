#! /bin/sh

dunst -config ~/.config/dunst/dunstrc \
  - "$(xgetres background)" \
  -lf "$(xgetres foreground)" \
  -nb "$(xgetres background)" \
  -nf "$(xgetres foreground)" \
  -cb "$(xgetres background)" \
  -cf "$(xgetres foreground)" \
  -bf "$(xgetres foreground)" \
  -frame_color "$(xgetres foreground)"
