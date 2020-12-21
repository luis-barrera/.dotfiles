#bin/bash

printf "*********** Pulling Notas ***********\n"
/usr/bin/rclone copy --no-traverse gdrivemounted:mount/Notas ~/Notas -i && \

printf "*********** Pulling SextoTrimestre ***********\n"
/usr/bin/rclone copy --no-traverse gdrivemounted:mount/SextoTrimestre ~/SextoTrimestre -i && \

printf "*********** Pulling .keepassxc ***********\n"
/usr/bin/rclone copy --no-traverse gdrivemounted:mount/Archivo/keepass ~/.keepass -i && \

printf "*********** Pulling Arduino ***********\n"
/usr/bin/rclone copy --no-traverse gdrivemounted:mount/Archivo/Arduino ~/Arduino -i
