#bin/bash

printf "*********** Pushing Notas ***********\n"
/usr/bin/rclone copy --max-age 24h --no-traverse ~/Notas gdrivemounted:mount/Notas -i && \

printf "*********** Pushing SextoTrimestre ***********\n"
/usr/bin/rclone copy --max-age 24h --no-traverse ~/SextoTrimestre gdrivemounted:mount/SextoTrimestre -i && \

printf "*********** Pushing .keepassxc ***********\n"
/usr/bin/rclone copy --max-age 24h --no-traverse ~/.keepass gdrivemounted:mount/Archivo/keepass -i && \

printf "*********** Pushing Arduino ***********\n"
/usr/bin/rclone copy --max-age 24h --no-traverse ~/Arduino gdrivemounted:mount/Archivo/Arduino -i
