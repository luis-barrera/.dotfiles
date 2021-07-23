#!/bin/bash
# Hard push de los archivos locales a Google Drive
# Copia todos los archivos sin checar primero por la fecha de creación

printf "*********** Pushing Notas ***********\n"
/usr/bin/rclone sync -i ~/Notas gdrivemounted:mount/Notas && \

printf "*********** Pushing SextoTrimestre ***********\n"
/usr/bin/rclone sync -i ~/SextoTrimestre gdrivemounted:mount/SextoTrimestre && \

printf "*********** Pushing .keepassxc ***********\n"
/usr/bin/rclone sync -i ~/.keepass gdrivemounted:mount/Archivo/keepass && \

printf "*********** Pushing Arduino ***********\n"
/usr/bin/rclone sync -i ~/Arduino gdrivemounted:mount/Archivo/Arduino && \

printf "*********** Pushing drivesync ***********\n"
/usr/bin/rclone sync -i ~/drivesync gdrivemounted:mount/drivesync
