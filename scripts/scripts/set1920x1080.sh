#!/bin/bash

# xrandr --newmode "1920x1080_60.00" 172.80 1920 2040 2248 2576 1080 1081 1084 1118 -HSync +Vsync &&
xrandr --addmode DP2 "1920x1080_60.00" &&
xrandr --output DP2 --mode "1920x1080_60.00" &&

xrandr --output eDP1 --primary --mode 1600x900 --pos 0x180 --rotate normal --output DP1 --off --output DP2 --mode 1920x1080_60.00 --pos 1600x0 --rotate normal --output HDMI1 --off --output HDMI2 --off --output VIRTUAL1 --off

# set touchpad configs
# Perfil de aceleracion, 0 es constante y 1 adaptativo
xinput set-prop 11 307 1 &&
# Presiones para detectar: el dedo salio, el dedo entro, dejarlo en 0
xinput set-prop 11 312 25, 40, 0 &&
# Tiempo para detectar un toque en touchpad
xinput set-prop 11 313 100 &&
# Tiempo para detectar un movimiento en touchpad
xinput set-prop 11 314 200 &&
# Distancia que se recorre con el scroll
xinput set-prop 11 320 80, 50 &&
# Activar scrool usando: borde derecho del touchpad, borde inferior del touchpad, poner un dedo en la esquina para poder hacer scroll
xinput set-prop 11 321 0, 0, 0 &&
# Permitir scroll a dos dedos: vertical, horizontal
xinput set-prop 11 322 1, 1 &&
# Factores de velocidad cuando: movemos lento el dedo, movemos rapido el dedo, un factor de cambio, <deprecated>
xinput set-prop 11 323 1.0, 2.0, 0.015, 0 &&
# Circular scrolling, hacer scrolling como en un iPod
xinput set-prop 11 329 1 &&
# El circular scrolling empieza desde el borde derecho
xinput set-prop 11 331 3 &&
# En lugar de usar los bordes usamos un elipse al interior del touchpad para hacer el scroll
xinput set-prop 11 332 1 &&
# Coasting sigue haciendo scroll por un momento después de dejar el touchpad
xinput set-prop 11 335 150, 50 &&

# set trackpoint configs
# Toggle del botón central para hacer scroll
xinput set-prop 12 356 0 &&
# Velocidad del apuntador
xinput set-prop 12 360 0.5 &&
# Perfiles de aceleración
xinput set-prop 12 363 0, 0
