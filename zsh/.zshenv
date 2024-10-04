# Loads ssh-agent and adds credentials
#eval "$(ssh-agent -s)"
#ssh-add ~/.ssh/github
export SSH_AUTH_SOCK

# # Java y WM
export AWT_TOOLKIT=MToolkit

if [ "$XDG_SESSION_DESKTOP" = "sway" ] ; then
    # https://github.com/swaywm/sway/issues/595
    export _JAVA_AWT_WM_NONREPARENTING=1
fi

PATH="/home/luisbarrera/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/home/luisbarrera/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/luisbarrera/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/luisbarrera/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/luisbarrera/perl5"; export PERL_MM_OPT;

# Rust
export PATH=$PATH:$HOME/.cargo/bin

# Emacs
export PATH=/home/luisbarrera/.emacs.d/bin:$PATH

export PATH=$PATH:/home/luisbarrera/.local/lib

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

# # Tray icon de batería
# source <(cbatticon)
# 
# # Abrir aplicaciones después del display server, pero antes del WM
# source <(playerctld daemon)
# source <(syncthingtray --wait)
# 
# # Load Angular CLI autocompletion.
# source <(ng completion script)
# 
# # Keyring
# source <(gnome-keyring-daemon --start)

# tmux layouts
# export PATH="$HOME/.tmuxifier/bin:$PATH"
# source <(tmuxifier init -)

# Composer, Laravel
export PATH="$HOME/.composer/vendor/bin:$PATH"
