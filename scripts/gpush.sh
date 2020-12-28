#bin/bash

printf "*********** Pushing Notas ***********\n"
/usr/bin/rclone sync -i --max-age 24h ~/Notas gdrivemounted:mount/Notas && \

printf "*********** Pushing SextoTrimestre ***********\n"
/usr/bin/rclone sync -i --max-age 24h ~/SextoTrimestre gdrivemounted:mount/SextoTrimestre && \

printf "*********** Pushing .keepassxc ***********\n"
/usr/bin/rclone sync -i --max-age 24h ~/.keepass gdrivemounted:mount/Archivo/keepass && \

printf "*********** Pushing Arduino ***********\n"
/usr/bin/rclone sync -i --max-age 24h ~/Arduino gdrivemounted:mount/Archivo/Arduino && \

printf "*********** Pushing drivesync ***********\n"
/usr/bin/rclone sync -i --max-age 24h ~/drivesync gdrivemounted:mount/drivesync
