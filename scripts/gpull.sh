#bin/bash

printf "*********** Pulling Notas ***********\n"
/usr/bin/rclone sync -i gdrivemounted:mount/Notas ~/Notas && \

printf "*********** Pulling SextoTrimestre ***********\n"
/usr/bin/rclone sync -i gdrivemounted:mount/SextoTrimestre ~/SextoTrimestre && \

printf "*********** Pulling .keepassxc ***********\n"
/usr/bin/rclone sync -i gdrivemounted:mount/Archivo/keepass ~/.keepass && \

printf "*********** Pulling Arduino ***********\n"
/usr/bin/rclone sync -i gdrivemounted:mount/Archivo/Arduino ~/Arduino
