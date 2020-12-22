#!usr/bin/bash

declare estado=$(playerctl status)
declare info="  Nada en reproducción  "

if [ "$estado" = "Playing" -o "$estado" = "Paused" ]; then
  declare player=$(playerctl metadata --format "{{ playerName }}")  
  declare song=$(playerctl metadata --format "{{ title }}")
  declare artist=$(playerctl metadata --format "{{ artist }}")

  if [ "$player" = "spotify" -o "$player" = "vlc" ]; then
    if [ ${#song} <= 50 ]; then
      echo " $player 🎧 $artist - $song "
    else
      echo " $player 🎧 $artist - ${song::40} "
    fi;
  else
    if [ ${#song} <= 50 ]; then
      echo " $player 🎧 $song "
    else
      echo " $player 🎧 ${song::40} "
    fi;
  fi;
else
  echo " $info "
fi;
