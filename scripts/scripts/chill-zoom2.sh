#!/usr/bin/bash -xe

# Cambiar los valores de AllowedCPUs y MemoryHigh de acuerdo a las necesidades

cat <<EOF > "${HOME}/.config/systemd/user/zoom.slice"
[Slice]
AllowedCPUs=0-4
MemoryHigh=6G
EOF

cat /usr/share/applications/Zoom.desktop | sed -E 's#^(Exec=).*$#Exec=/usr/bin/systemd-run --user --slice=zoom.slice /opt/zoom/ZoomLauncher#' > "${HOME}/.local/share/applications/Zoom.desktop"

update-desktop-database ~/.local/share/applications
