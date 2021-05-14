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
  autocmd VimEnter * PlugInstall
endif

call plug#begin('~/.config/nvim/autoload/plugged')
  " Autocompletado
    Plug 'neovim/nvim-lspconfig'
    Plug 'hrsh7th/nvim-compe'
    Plug 'hrsh7th/vim-vsnip'
    Plug 'hrsh7th/vim-vsnip-integ'
  " Cool Icons
    Plug 'ryanoasis/vim-devicons'
  " Cierra los parentesis, llaves y corchetes
    Plug 'jiangmiao/auto-pairs'
  " Multi-cursor
    Plug 'mg979/vim-visual-multi', {'branch': 'master'}
  " Guarda la posición del cursor
    Plug 'vim-scripts/restore_view.vim'
  " Scroll suave
    Plug 'psliwka/vim-smoothie'
  " Desactiva las mayúsculas cuando se entra al modo insert
    Plug 'suxpert/vimcaps'
  " Cambia surround con cs{simbolo actual}{simbolo deseado}
    Plug 'tpope/vim-surround'
  " Algunos comandos de UNIXShell en nvim nativamente
    Plug 'tpope/vim-eunuch'
  " Syntax para varios lenguajes
    Plug 'sheerun/vim-polyglot'
  " Sopore para comandos Git en nvim
    Plug 'tpope/vim-fugitive'
  " Muestra información de los commits con <leader>gm
    Plug 'rhysd/git-messenger.vim'
  " Soporte para Github en nvim
    Plug 'tpope/vim-rhubarb'
  " Muestra el historial de commits con :GV o :GV!
    Plug 'junegunn/gv.vim'
  " Snippets para webdev
    Plug 'mattn/emmet-vim'
  " Snippets para varios lenguajes
  " Si hay problemas para instalar snipmate por primera vez, comentar la linea de snipmate para instalar primero sus dependencias
    " Plug 'honza/vim-snippets'
  " Dependencia para snipmate
    " Plug 'marcweber/vim-addon-mw-utils'
    " Plug 'tomtom/tlib_vim'
  " Plugin para usar snippets
    " Plug 'garbas/vim-snipmate'
  " Comentarios con el keymap gcc
    Plug 'tpope/vim-commentary'
  " VimWiki
    Plug 'vimwiki/vimwiki'
  " Página de inico
    " Plug 'mhinz/vim-startify'
  " Muestra espacios en blanco innecesarios
    Plug 'ntpeters/vim-better-whitespace'
  " Muestra los keymaps presionando <space>
    Plug 'liuchengxu/vim-which-key'
  " Muestra salida del codigo mientras se escribe
    Plug 'metakirby5/codi.vim'
  " Hace más facil la modificación de tags HTML
    Plug 'AndrewRadev/tagalong.vim'
  " Cierra Automáticamente tags HTML
    Plug 'alvan/vim-closetag'
  " Muestra los colores #ffffff
    Plug 'norcalli/nvim-colorizer.lua'
  " Resalta posibles movimientos con f, F, t, T
    Plug 'unblevable/quick-scope'
  " Paréntesis de colores
    Plug 'kien/rainbow_parentheses.vim'
  " Muestra los diff de git en la signcolumn
    Plug 'airblade/vim-gitgutter'
  " <F5> para ver historial cosas borradas
    Plug 'mbbill/undotree'
  " nvim sin distracciones
    Plug 'junegunn/goyo.vim'
  " Soporte para latex
    Plug 'lervag/vimtex'
  " git-blame
    Plug 'zivyangll/git-blame.vim'
  " Autocompletado
    " Plug 'neoclide/coc.nvim', {'branch': 'release'}
  " Detectar mejor las palabras al usar w, e & b
    " Plug 'chaoren/vim-wordmotion'
  " Preview de un .tex con el command LLP
    " Plug 'xuhdev/vim-latex-live-preview', {'for':'tex'}

  " Themes
    Plug 'itchyny/lightline.vim'
    " Plug 'kaicataldo/material.vim'
    Plug 'morhetz/gruvbox'
    Plug 'bluz71/vim-moonfly-colors'
    Plug 'savq/melange'
    Plug 'jaredgorski/spacecamp'
call plug#end()

" Automatically install missing plugins on startup
autocmd VimEnter *
  \  if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \|   PlugInstall --sync | q
  \| endif
