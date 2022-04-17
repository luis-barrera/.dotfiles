#! /usr/bin/env bash

set -xe

# Script para que Zoom no consuma tantos recursos, limitanto con cgroups los
# máximos recursos asignados
# Obtenido de https://gist.github.com/abraithwaite/1d78a946f90be478faedb5ca4db6d62e

main() (
  allowedCpus="${1?Missing allowed CPUs, ex: 0-4}"
  maxMemory="${2?Missing max memory, ex: 6G}"

  configFolder="${HOME}/.config/systemd/user/"
  mkdir -p "${configFolder}"
  
  # Add parameter checks to avoid invalid values here
  {
    printf '[Slice]\n'
    printf 'AllowedCPUs=%s\n' "${allowedCpus}"
    printf 'MemoryHigh=%s\n' "${maxMemory}"
  } > "${configFolder}/zoom.slice"
  
  awk '{ 
    if ($0 ~ /^Exec=/) 
      print "Exec=/usr/bin/systemd-run --user --slice=zoom.slice /opt/zoom/ZoomLauncher"; 
    else 
      print; 
  }' < /usr/share/applications/Zoom.desktop > "${HOME}/.local/share/applications/Zoom.desktop"
    
  update-desktop-database ~/.local/share/applications
)

main "$@"
