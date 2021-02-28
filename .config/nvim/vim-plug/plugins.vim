"           __            _
"    ____  / /_  ______ _(_)___  _____
"   / __ \/ / / / / __ `/ / __ \/ ___/
"  / /_/ / / /_/ / /_/ / / / / (__  )
" / .___/_/\__,_/\__, /_/_/ /_/____/
"/_/            /____/

" auto-install vim-plug
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  "autocmd VimEnter * PlugInstall
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

call plug#begin('~/.config/nvim/autoload/plugged')
    Plug 'ryanoasis/vim-devicons'                       " Cool Icons
    Plug 'jiangmiao/auto-pairs'                         " Cierra los parentesis, llaves y corchetes
    " Plug 'xuhdev/vim-latex-live-preview', {'for':'tex'} " Preview de un .tex con el command LLP
    Plug 'mg979/vim-visual-multi', {'branch': 'master'} " Multi-cursor
    Plug 'vim-scripts/restore_view.vim'                 " Guarda la posición del cursor
    Plug 'psliwka/vim-smoothie'                         " Scroll suave
    Plug 'suxpert/vimcaps'                              " Desactiva las mayúsculas cuando se entra al modo insert
    Plug 'tpope/vim-surround'                           " Cambia surround con cs{simbolo actual}{simbolo deseado}
    Plug 'tpope/vim-eunuch'                             " Algunos comandos de UNIXShell en nvim nativamente
    Plug 'sheerun/vim-polyglot'                         " Syntax para varios lenguajes
    Plug 'tpope/vim-fugitive'                           " Sopore para comandos Git en nvim
    Plug 'rhysd/git-messenger.vim'                      " Muestra información de los commits con <leader>gm
    Plug 'tpope/vim-rhubarb'                            " Soporte para Github en nvim
    Plug 'junegunn/gv.vim'                              " Muestra el historial de commits con :GV o :GV!
    Plug 'mattn/emmet-vim'                              " Snippets para webdev
    Plug 'honza/vim-snippets'                           " Snippets para varios lenguajes
    Plug 'marcweber/vim-addon-mw-utils'                 " Dependencia para snipmate
    Plug 'tomtom/tlib_vim'                              " Dependencia para snipmate
    " Si hay problemas para instalar snipmate por primera vez, comentar la linea de snipmate para instalar primero sus dependencias
    Plug 'garbas/vim-snipmate'                          " Plugin para usar snippets
    Plug 'tpope/vim-commentary'                         " Comentarios con el keymap gcc
    Plug 'vimwiki/vimwiki'                              " Vim Wiki
    Plug 'neoclide/coc.nvim', {'branch': 'release'}     " Autocompletado
    Plug 'mhinz/vim-startify'                           " Página de inico
    Plug 'ntpeters/vim-better-whitespace'               " Muestra espacios en blanco innecesarios
    Plug 'liuchengxu/vim-which-key'                     " Muestra los keymaps presionando <space>
    Plug 'metakirby5/codi.vim'                          " Muestra salida del codigo mientras se escribe
    Plug 'AndrewRadev/tagalong.vim'                     " Hace más facil la modificación de tags HTML
    Plug 'alvan/vim-closetag'                           " Cierra Automáticamente tags HTML
    Plug 'norcalli/nvim-colorizer.lua'                  " Muestra los colores #ffffff
    Plug 'unblevable/quick-scope'                       " Resalta posibles movimientos con f, F, t, T
    Plug 'kien/rainbow_parentheses.vim'                 " Paréntesis de colores
    Plug 'airblade/vim-gitgutter'                       " Muestra los diff de git en la signcolumn
    Plug 'mbbill/undotree'                              " <F5> para ver historial cosas borradas
    Plug 'junegunn/goyo.vim'                            " nvim sin distracciones
    Plug 'lervag/vimtex'                                " Soporte para latex
    Plug 'chaoren/vim-wordmotion'                       " Detectar mejor las palabras al usar w, e & b
    Plug 'zivyangll/git-blame.vim'                      " git-blame

    " Themes
    Plug 'itchyny/lightline.vim'
    " Plug 'kaicataldo/material.vim'
    Plug 'morhetz/gruvbox'
    Plug 'bluz71/vim-moonfly-colors'

    call plug#end()

" Automatically install missing plugins on startup
autocmd VimEnter *
  \  if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \|   PlugInstall --sync | q
  \| endif
