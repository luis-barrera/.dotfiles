#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

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

codi() {
   local syntax="${1:-python}"
   shift
   nvim -c \
     "let g:startify_disable_at_vimenter = 1 |\
     set bt=nofile ls=0 noru nonu nornu |\
     hi CodiVirtualText guifg=red
     hi ColorColumn ctermbg=NONE |\
     hi VertSplit ctermbg=NONE |\
     hi NonText ctermfg=0 |\
     Codi $syntax" "$@"
}
# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'

# Java for Flutter
# JAVA_OPTS='-XX:+IgnoreUnrecognizedVMOptions --add-modules java.se.ee'

export ANDROID_SDK_ROOT=/home/luisbarrera/Android/Sdk

# JavaFX
export PATH_TO_FX=/home/luisbarrera/eclipse-ws-java/javafx-sdk-16/lib

# MySQL
MYSQL_TCP_PORT=3306
export MYSQL_TCP_PORT
PATH=${PATH}:/usr/local/mysql/bin

# Rust
export PATH=$PATH:$HOME/.cargo/bin

# Javascript/Typescript lsp
export DENO_INSTALL="/home/luisbarrera/.deno"
export PATH="$DENO_INSTALL/bin:$PATH"

# Editor por defecto
export EDITOR=vim

# Arreglar problemas de aplicaciones java en bspwm
# export AWT_TOOLKIT=MToolkit
export _JAVA_AWT_WM_NONREPARENTING=1

# Keyring
if [ -n "$DESKTOP_SESSION" ];then
    eval $(gnome-keyring-daemon --start)
    export SSH_AUTH_SOCK
fi
