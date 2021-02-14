# pywal colorscheme
# Import colorscheme from 'wal' asynchronously
# &   # Run the process in the background.
# ( ) # Hide shell job control messages.
cat ~/.cache/wal/sequences &
# Alternative (blocks terminal for 0-3ms)
cat ~/.cache/wal/sequences
# To add support for TTYs this line can be optionally added.
# source ~/.cache/wal/colors-tty.sh

# fetch
pfetch &&

# alias
# Configurar AwesomeWM
alias wmrc="nvim ~/.config/awesome/rc.lua"
# Configuar tema de AwesomeWM
alias themewm="nvim ~/.config/awesome/theme.lua"
# Configuar xmonad
alias xmwm="nvim ~/.xmonad/xmonad.hs"
# Corre Script para usar un monitor
alias onescreen="sh ~/scripts/onescreenlayout.sh"
# Corre Script para usar dos monitores
alias twoscreens="sh ~/scripts/twoscreenslayout.sh"
# Abre jupyter notebook
alias jn="jupyter-notebook"
alias jl="jupyter-lab"
# Alias para usar MariaDB
# alias startmysql="systemctl enable mysqld.service --now"
# alias restartmysql="systemctl restart mysqld.service"
# alias stopmysql="systemctl disable mysqld.service --now"
# Abrir configuración de nvim
alias nvimconfig="nvim ~/.config/nvim/init.vim"
# Administrar plugins de nvim
alias nvimplugins="nvim ~/.config/nvim/vim-plug/plugins.vim"
# Configuración básica de vim y nvim
alias vimrc="vim ~/.vimrc"
# Abre otra terminal
alias kitty="kitty --detach"
# Abre zathura de manera correcta
alias zathura="zathura --fork"
# Perfil de privacidad para firefox
alias firefoxprofile="firefox -no-remote -P privacy-profile"
# Alias para bare repo de mis dotfiles
alias dots="/usr/bin/git --git-dir=$HOME/dotfiles --work-tree=$HOME"
# Ejecutar jamovi
alias jamovi="flatpak run org.jamovi.jamovi"
# Pull de varias carpetas locales a remoto de google drive
alias gpull="sh $HOME/scripts/gpull.sh"
alias gpush="sh $HOME/scripts/gpush.sh"
# Ver imágenes en el directorio actual
alias images="viewnior ./"
# Abre el stream de lofi en mpv
alias lofi="mpv https://youtu.be/5qap5aO4i9A"
alias jrap="mpv https://youtu.be/Ii-_EtWBVSM"
# Siguiendo el meme
alias l='ls'
alias s='ls'
alias sl='ls'
alias lls='ls'
alias lsl='ls'
alias lss='ls'
alias sll='ls'
alias sls='ls'

starship init fish | source
