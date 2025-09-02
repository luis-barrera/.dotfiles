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
