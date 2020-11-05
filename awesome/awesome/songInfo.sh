#!usr/bin/bash

declare estado=$(playerctl status)
declare info="Nada en reproducción"

if [ "$estado" = "Playing" -o "$estado" = "Paused" ]; then
  declare player=$(playerctl metadata --format "{{ playerName }}")  
  declare song=$(playerctl metadata --format "{{ title }}")
  declare artist=$(playerctl metadata --format "{{ artist }}")

  if [ "$player" = "firefox" -o "$player" = "chromium" ]; then
    echo "  $player 🎧 $song "
  else
    echo "  $player 🎧 $artist - $song "
  fi;
else
  echo $info 
fi;
