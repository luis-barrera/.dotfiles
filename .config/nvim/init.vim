"    ____      _ __        _
"   /  _/___  (_) /__   __(_)___ ___
"   / // __ \/ / __/ | / / / __ `__ \
" _/ // / / / / /__| |/ / / / / / / /
"/___/_/ /_/_/\__(_)___/_/_/ /_/ /_/

" General Settings
source $HOME/.config/nvim/vim-plug/plugins.vim                  " Plugins
source $HOME/.vimrc                                             " Exporta la config de vim
source $HOME/.config/nvim/keys/mappings.vim                     " Mapeos de teclas
source $HOME/.config/nvim/plug-config/quickscope.vim            " Ayuda a identificar cuando usamos f, F, t & T

" Configuración de algunos plugins
source $HOME/.config/nvim/plug-config/vim-commentary.vim        " Comenta y descomenta líneas con gcc
source $HOME/.config/nvim/plug-config/latex-live-preview.vim    " Con el comando :LLP muestra la salida de un archivo LaTeX
source $HOME/.config/nvim/plug-config/lexical.vim               " Analizador de ortografía
source $HOME/.config/nvim/plug-config/better-whitespace.vim     " Resalta en #111111 espacios blancos innecesarios

source $HOME/.config/nvim/plug-config/fzf.vim
source $HOME/.config/nvim/plug-config/sneak.vim
source $HOME/.config/nvim/plug-config/codi.vim
source $HOME/.config/nvim/plug-config/vim-wiki.vim
source $HOME/.config/nvim/plug-config/coc.vim
source $HOME/.config/nvim/plug-config/goyo.vim
source $HOME/.config/nvim/plug-config/vim-rooter.vim
source $HOME/.config/nvim/plug-config/start-screen.vim
source $HOME/.config/nvim/plug-config/gitgutter.vim
source $HOME/.config/nvim/plug-config/git-messenger.vim
source $HOME/.config/nvim/plug-config/closetags.vim
source $HOME/.config/nvim/plug-config/floaterm.vim
source $HOME/.config/nvim/plug-config/far.vim
source $HOME/.config/nvim/plug-config/tagalong.vim
source $HOME/.config/nvim/plug-config/bracey.vim
source $HOME/.config/nvim/plug-config/asynctask.vim
source $HOME/.config/nvim/plug-config/window-swap.vim
source $HOME/.config/nvim/plug-config/markdown-preview.vim
source $HOME/.config/nvim/plug-config/rainbow_parentheses.vim
source $HOME/.config/nvim/plug-config/nvim-colorizer.vim
source $HOME/.config/nvim/keys/which-key.vim                    " Muestra las combinaciones de teclas de la barra esp.

" source $HOME/.config/nvim/plug-config/vim-airline.vim
" source $HOME/.config/nvim/plug-config/rainbow.vim
" source $HOME/.config/nvim/plug-config/vista.vim
" source $HOME/.config/nvim/plug-config/xtabline.vim
" source $HOME/.config/nvim/plug-config/illuminate.vim
" luafile $HOME/.config/nvim/lua/plug-colorizer.lua
" source $HOME/.config/nvim/plug-config/vimspector.vim " Uncomment if you want to use Vimspector
" source $HOME/.config/nvim/plug-config/ale.vim

" Add paths to node and python here
if !empty(glob("~/.config/nvim/paths.vim"))
  source $HOME/.config/nvim/paths.vim
endif

" Temas
"source $HOME/.config/nvim/themes/material.vim
source $HOME/.config/nvim/themes/gruvbox.vim
