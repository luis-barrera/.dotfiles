# Setup for laptop

1. Install arch linux with `archinstall and a desktop env (hyprland).
1. Install basic software:
    - vim
    - nvim
    - vi
    - firefox
    - git
    - stow
    - kitty
    - ghostty
    - zsh
    - tmux
1. Setup zsh and oh-my-shell.
1. Fix suspend:
    1. Copy `scripts/fix-suspend.sh` to `/usr/local/sbin`.
    1. Copy `scripts/fix-suspend.service` to `/etc/systemd/system/`.
    1. Run `systemctl daemon-reload`.
    1. Run `systemctl --now enable fix-suspend.service`.
1. Install packages from `pacman-packages.txt` and `aur-packages.txt`:
    - `sudo pacman -S --needed - < pacman-packages.txt`
    - `paru -S --needed - < aur-packages.txt`
1. Install `emacs` from pacman, then install `doom emacs` from GitHub.
1. Setup google-drive:
    - Install `rclone` from pacman.
    - (Optional) If already have setup in another machine, copy the `.config/rclone/rclone.config`, if not execute the next step.
    - set up using your Google Account (a Developer Console is needed) rclone docs have a guide for that
    - copy the service `~/scripts/rclone-google-drive.service` to `.config/systemd/user`
    - load the service to systemctl: `systemctl --user daemon-reload`
    - enable the service: `systemctl --user enable --now rclone-google-drive.service`.
