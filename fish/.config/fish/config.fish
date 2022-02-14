pfetch

# cat ~/.cache/wal/sequences
# wal -q --theme sexy-brewer
# wal -q -t sexy-dwmrob
# source ~/.cache/wal/colors-tty.sh

if status is-interactive
    # Commands to run in interactive sessions can go here
end

starship init fish | source
zoxide init fish | source
