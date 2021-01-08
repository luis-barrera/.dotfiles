# pywal colorscheme
# Import colorscheme from 'wal' asynchronously
# &   # Run the process in the background.
# ( ) # Hide shell job control messages.
(cat ~/.cache/wal/sequences &)
# Alternative (blocks terminal for 0-3ms)
cat ~/.cache/wal/sequences
# To add support for TTYs this line can be optionally added.
source ~/.cache/wal/colors-tty.sh

# fetch
(pfetch &&)

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/home/luisbarrera/.oh-my-zsh"

# ZSH_THEME="typewritten"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

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
HIST_STAMPS="dd.mm.yyyy"

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git colored-man-pages)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
   export EDITOR='nvim'
else
   export EDITOR='vim'
fi

# Compilation flags
export ARCHFLAGS="-arch x86_64"

#############################################################################################################
############## Aliases ######################################################################################
#############################################################################################################
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
alias themewm="nvim ~/.config/awesome/theme.lua"
# Corre Script para usar un monitor
alias onescreen="sh ~/scripts/onescreenlayout.sh"
# Corre Script para usar dos monitores
alias twoscreens="sh ~/scripts/twoscreenslayout.sh"
# Abre jupyter notebook
alias jn="jupyter-notebook"
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
alias dotfiles="/usr/bin/git --git-dir=$HOME/dotfiles --work-tree=$HOME"
# Función para grabar pantalla, toma el nombre del video de salida como parámetro (es necesario pulseaudio)
grabar() {
  if [ "$1" != "" ]
  then
    sh /home/luisbarrera/scripts/grabar.sh $1
  fi
}
# Ejecutar jamovi
alias jamovi="flatpak run org.jamovi.jamovi"
# Pull de varias carpetas locales a remoto de google drive
alias gpull="sh $HOME/scripts/gpull.sh"
alias gpush="sh $HOME/scripts/gpush.sh"
# Ver imágenes en el directorio actual
alias images="viewnior ./"
# Abre el stream de lofi en mpv
alias lofi="mpv https://youtu.be/5qap5aO4i9A"
alias lofimex="mpv --loop-playlist=inf https://youtu.be/VzCXIqufgKk"
# Siguiendo el meme
alias l='ls'
alias s='ls'
alias sl='ls'
alias lls='ls'
alias lsl='ls'
alias lss='ls'
alias sll='ls'
alias sls='ls'

source ~/.powerlevel10k/powerlevel10k.zsh-theme

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

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

# Codi, extensión de nvim
# Usage: codi [filetype] [filename]
codi() {
  local syntax="${1:-python}"
  shift
  nvim -c \
    "let g:startify_disable_at_vimenter = 1 |\
    set bt=nofile ls=0 noru nonu nornu |\
    hi ColorColumn ctermbg=NONE |\
    hi VertSplit ctermbg=NONE |\
    hi NonText ctermfg=0 |\
    Codi $syntax" "$@"
}
