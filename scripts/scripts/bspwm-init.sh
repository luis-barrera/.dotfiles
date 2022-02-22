#!/bin/bash

# Script para abrir aplicaciones en bspwm, dentro de bspwm tengo la reglas de a
# dónde deben ir cada uno de las windows

# Si Firefox no está abierto, abrirlo en modo nohup
pgrep firefox || nohup firefox &
sleep 0.25

# Si wisdowm-tree no está abierto, abrir una terminal con el programa, ya que
# es una aplicación de terminal
pgrep wisdom-tree || kitty --detach wisdom-tree &
sleep 0.25

# Igual Emacs
pgrep emacs || nohup emacs &
sleep 0.25

# btop, igual que wisdom-tree
pgrep btop || kitty --detach btop &
sleep 0.25

# Lo mismo para spotify, spotify no obedece las reglas por alguna razón.
pgrep spotify || (bspc desktop --focus f && (nohup spotify &))
