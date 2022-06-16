# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH
export PATH=/home/luisbarrera/.local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/home/luisbarrera/.oh-my-zsh"

# Set colorscheme using pywal
(cat ~/.cache/wal/sequences &)
# (wal -q --theme base16-seti &)
# (wal -q --theme sexy-x-dotshare &)
# (wal -q --theme sexy-sexcolors &)
# (wal -q --theme sexy-dwmrob &)
# (wal -q --theme gruvbox &)
# (wal -q --theme sexy-orangish &)
# (wal -q --theme sexy-digerati &)
# (wal -q --theme sexy-brewer &)
# (wal -q --theme sexy-parker_brothers &)

# pfetch
(pfetch &&)

# Prompt
eval "$(starship init zsh)"


# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
# ZSH_THEME="powerlevel10k/powerlevel10k"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# Caution: this setting can cause issues with multiline prompts (zsh 5.7.1 and newer seem to work)
# See https://github.com/ohmyzsh/ohmyzsh/issues/5765
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git
	alias-finder
	colored-man-pages
	fancy-ctrl-z
	fzf
	ripgrep
	zsh-interactive-cd
	zsh-autosuggestions
	)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

#####################################
############## Aliases ##############
#####################################
#
# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Configurar zsh
alias zshconfig="nvim ~/.zshrc"
# Configrar ohmyzsh
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Configurar AwesomeWM
alias wmrc="nvim ~/.config/awesome/rc.lua"
# Configuar tema de AwesomeWM
# alias themewm="nvim ~/.config/awesome/space-theme.lua"
alias themewm="nvim ~/.config/awesome/xresources-theme.lua"
# Configuar xmonad
alias xmwm="nvim ~/.xmonad/xmonad.hs"

# Corre Script para usar un monitor
alias onescreen="sh ~/scripts/onescreenlayout.sh"
# Corre Script para usar dos monitores
alias rightscreen="sh ~/scripts/rightscreen.sh"
alias leftscreen="sh ~/scripts/leftscreen.sh"
alias upscreen="sh ~/scripts/upscreen.sh"

# Abre jupyter notebook
alias jn="jupyter-notebook"
alias jl="jupyter-lab"

# Alias para usar MariaDB
alias startmysql="systemctl enable mysqld.service --now"
alias restartmysql="systemctl restart mysqld.service"
alias stopmysql="systemctl disable mysqld.service --now"
alias mysqld="sudo /usr/local/mysql/bin/mysqld_safe --user=mysql"
alias killmysqld="killall mysqld_safe"

# Abrir configuración de nvim
alias nvimconfig="cd ~/.config/nvim && nvim ~/.config/nvim/init.vim"
# Administrar plugins de nvim
alias nvimplugins="cd ~/.config/nvim && nvim ~/.config/nvim/vim-plug/plugins.vim"
# Configuración básica de vim y nvim
alias vimrc="vim ~/.vimrc"

# Abre otra terminal
alias kitty="kitty --detach"
alias nn="kitty --detach"

# Abre zathura de manera correcta
alias zathura="zathura --fork"

# Perfil de privacidad para firefox
alias firefoxprofile="firefox -no-remote -P privacy-profile"

# Alias para bare repo de mis dotfiles
# alias dots="/usr/bin/git --git-dir=$HOME/dotfiles --work-tree=$HOME"

# Función para grabar pantalla, toma el nombre del video de salida como parámetro (es necesario pulseaudio)
grabar() {
  if [ "$1" != "" ]
  then
    sh /home/luisbarrera/scripts/grabar.sh $1
  fi
}

# Ejecutar jamovi
# alias jamovi="flatpak run org.jamovi.jamovi"

# Ejecutar news flash
# alias rss="flatpak run com.gitlab.newsflash"

# Pull de varias carpetas locales a remoto de google drive
alias gpull="sh $HOME/scripts/gpull.sh"
alias gpush="sh $HOME/scripts/gpush.sh"

# Ver imágenes en el directorio actual
alias images="viewnior ./"

# Abre el stream de lofi en mpv
# Utiliza youtube-dl para obtener una calidad no tan alta, por defecto se usa la mejor calidad posible
# Para obtener el codigo del formato, usar youtube-dl --list-formats <URL>
alias lofi="mpv --ytdl-format=94 https://youtu.be/5qap5aO4i9A"
# Reproduce sonidos de ruido blanco desde youtube
alias whitenoise="mpv --ytdl-format=251 https://youtu.be/nMfPqeZjc2c"
# Reproduce sonidos relajantes de la selva
alias relax="mpv --ytdl-format=251 https://youtu.be/cjkFG6bHGNc"
alias travelers="mpv --ytdl-format=251 https://youtu.be/mDZf-fjBxz4"
alias heavy-classic="mpv --ytdl-format=251 https://youtu.be/2QKuxt_QKts"

# Siguiendo el meme
alias l='ls'
alias s='ls'
alias sl='ls'
alias lls='ls'
alias lsl='ls'
alias lss='ls'
alias sll='ls'
alias sls='ls'

# Kittens, plugins para kitty
# alias ssh='kitty +kitten ssh'

# Abrir archivos de la escuela en zathura
alias horario="zathura ~/Trim10-22I/Horario21I.pdf"
alias caluam="zathura ~/Trim10-22I/caluam.pdf"
# alias librogrupos="zathura ~/Trim-7/Grupos/Judson-Abstract_algebra.pdf"
# alias librogruposesp="zathura ~/Trim-7/Grupos/Algebra_abstracta-Judson.pdf"
alias flutterbook="zathura ~/Pendientes/Flutter\ for\ Beginners\ by\ Alessandro\ Biessek\ \(z-lib.org\).pdf"
alias tsbook="zathura ~/Pendientes/Programming\ TypeScript\ by\ Boris\ Cherny\ \(z-lib.org\).epub"

# Abrir AppImages
alias pizarron="~/AppImages/OpenBoard-4fca3a6-x86_64.AppImage"

# Abrir nvim en VimWiki
alias vimwiki="cd ~/Notas && nvim -c VimwikiIndex"
alias wiki="cd ~/Notas && nvim -c VimwikiIndex"

# Alias a oneliners
alias toimg="convert label:@- process.png" # después de un comando, hacer pip a este alias covierte la salida a una imágen
alias ipdir="curl ipinfo.io" # Muestra la ip
alias contribuidores="git log --format='%aN' | sort -u" # Muestra ordenandamente los contribuidores de un repo git
alias common-comands="history | awk '{print $2}' | sort | uniq -c | sort -rn | head" # Comandos más comunes
alias lsd="ls -d */" # Lista solo los directorios
alias bigger-dirs="du -hs */ | sort -hr | head" # Lista los directorios más grandes
alias internet-inspect="ss -p" # Muestra las apps que están usando internet
# alias rm-excepto="rm -f !(test.txt)" # Borra todos los archivos, excepto el que le demos
alias basic-server="python3 -m http.server" # Crea un server básico para poder compartir archivos sobre la red

# Sustituir ls con exa
alias la="exa -la --icons --group-directories-first -h --git"
alias ls="exa -l --icons --group-directories-first -h"
# Sustituir cat con bat
alias cat="bat"
# Sustituir ps por procs
# alias ps="procs"
# Sustituir sed por sd
# alias sed="sd"
# Sustituir du por dust
alias du="dust"

# Translate
alias esen="trans -s es -t en"
alias enes="trans -s en -t es"
alias dees="trans -s de -t es"
alias esde="trans -s es -t de"

# Anaconda environment
alias conda-activate="source /opt/anaconda/bin/activate root"
alias conda-deactivate="source /opt/anaconda/bin/deactivate root"

# Ver webcam en una ventana usando mpv
alias webcam="mpv av://v4l2:/dev/video0 --profile=low-latency --untimed"

# Ver qué proceso está usando mucha memoria
alias psmem='ps auxf | sort -nr -k 4'
alias psmem10='ps auxf | sort -nr -k 4 | head -10'

# Ver qué procesos estan usando mucho CPU
alias pscpu='ps auxf | sort -nr -k 3'
alias pscpu10='ps auxf | sort -nr -k 3 | head -10'

# ripgrep, buscar pero ignorando si son mayúsculas o minúsculas
alias rg='rg -i'

# Alias de lua
alias luamake="/home/luisbarrera/dev/lua-language-server/3rd/luamake/luamake"

# Alias para libreoffice
alias office="libreoffice"

# Alias para establecer web browser por default
alias set-firefox-default-browser="xdg-settings set default-web-browser firefox.desktop"
alias set-chromium-default-browser="xdg-settings set default-web-browser chromium.desktop"

# Alias apagar el wifi
alias wifi-down="sudo ip link set wlan0 down"
alias wifi-up="sudo ip link set wlan0 up"

# Entrar al home server
alias homer="ssh 192.168.0.8"

# Desactivar redes sociales editando el /etc/hosts
alias sociales-off="sudo sed -i '/.*facebook.com.*/c\0.0.0.0 www.facebook.com' /etc/hosts"
alias sociales-on="sudo sed -i '/.*facebook.com.*/c\#0.0.0.0 www.facebook.com' /etc/hosts"

# Montar el server samba
alias samba="sudo mount -t cifs //192.168.0.8/valbar /mnt/samba-hp -o username=valbar"

# Subir archivos locales a Google Drive
# alias drive-push="cd ~/google-drive && drive push --ignore-name-clashes --hidden org-mode org-roam Trim10-22I"
alias drive-push="cd ~/google-drive && drive push --ignore-name-clashes --hidden org-mode org-roam"

# Alias para pomodoro
alias pomato="cd ~/dev/pomato && python pomato.py"

# Alias navegador para desarrollo web
alias dev-browser="firefox-developer-edition"

# Alias terminar procesos por nombre
alias ka="killall"

# Alias de ledger
alias bal="ledger balance --file org-mode/finances.ledger"

# Alias de code
alias code="code ./"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
# [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# Establece un navegador web por defecto
# export BROWSER=/sbin/vivaldi-stable

# Refresca el tiempo de espera entre cada uso de sudo
alias sudo='sudo -v; sudo '
# Mensaje personalizado de sudo, el principio del prompt es el char para un sonido
# export SUDO_PROMPT="Necesitas la contraseña para hacer eso, pendejo: "

# Secrets para spotify-dl
# source ~/.spotify-secrets

PATH="/home/luisbarrera/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/home/luisbarrera/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/luisbarrera/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/luisbarrera/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/luisbarrera/perl5"; export PERL_MM_OPT;

# opam configuration
[[ ! -r /home/luisbarrera/.opam/opam-init/init.zsh ]] || source /home/luisbarrera/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null

# Rust
export PATH=$PATH:$HOME/.cargo/bin

# Emacs
export PATH=/home/luisbarrera/.emacs.d/bin:$PATH

export PATH=$PATH:/home/luisbarrera/.local/lib


# PHP 7
# export PATH=/sbin/php7:$PATH
# alias php=/sbin/php7

# Zoxide, reemplazo para cd
eval "$(zoxide init zsh)"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/luisbarrera/anaconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/luisbarrera/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/home/luisbarrera/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/luisbarrera/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

# Keyring
if [ -n "$DESKTOP_SESSION" ];then
    eval $(gnome-keyring-daemon --start)
    export SSH_AUTH_SOCK
fi
export DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus
source /usr/share/nvm/init-nvm.sh


# Load Angular CLI autocompletion.
source <(ng completion script)
