#bin/bash

if [ "$1" != "" ]
then
  /usr/bin/ffmpeg -video_size 1600x900 -framerate 20 -f x11grab -i :0.0 \
  -f pulse -ac 1 -i alsa_input.pci-0000_00_1b.0.analog-stereo           \
  -f pulse -ac 2 -i alsa_output.pci-0000_00_1b.0.analog-stereo.monitor  \
  -filter_complex amix=inputs=2 $1.mkv
else
  echo " No se ha proporcionado nombre de la salida "
fi
