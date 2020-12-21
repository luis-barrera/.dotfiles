#bin/bash

printf "*********** Pulling Notas ***********\n"
/usr/bin/rclone copy --max-age 24h --no-traverse ~/Notas gdrivemounted:mount/Notas -i && \

printf "*********** Pulling SextoTrimestre ***********\n"
/usr/bin/rclone copy --max-age 24h --no-traverse ~/SextoTrimestre gdrivemounted:mount/SextoTrimestre -i && \

printf "*********** Pulling .keepassxc ***********\n"
/usr/bin/rclone copy --max-age 24h --no-traverse ~/.keepass gdrivemounted:mount/Archivo/keepass -i && \

printf "*********** Pulling Arduino ***********\n"
/usr/bin/rclone copy --max-age 24h --no-traverse ~/Arduino gdrivemounted:mount/Archivo/Arduino -i
