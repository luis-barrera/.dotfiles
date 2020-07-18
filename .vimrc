" - Avoid using standard Vim directory names like 'plugin'
set laststatus=2

"para obtener highlights de algunos lenguajes de programación
syntax on

"no sonido en caso de error
set noerrorbells
"configuracón del tabulador: tamaño, y como cenvertir cierta cantidad de espacios a un tab
set tabstop=2 softtabstop=2
set shiftwidth=2
set expandtab
"indentado
set smartindent
"muestra el número de lineas 
set nu
"wrap de la linea
set nowrap
"sensibilidad a las mayúsculas
set smartcase
"evita crear archivos swap en el proyecto
set noswapfile
set nobackup
"historial de eliminación
set undodir=~/.vim/undodir
set undofile
"el sig nos sirve para ir marcando lo que vamos buscando
set incsearch

"configuración del colorscheme
let g:gruvbox_contrast_dark='medium'
let g:gruvbox_number_column='bg2'

call plug#begin('~/.vim/plugged')
"color scheme
Plug 'https://github.com/gryf/wombat256grf.git'
Plug 'morhetz/gruvbox'

Plug 'jremmen/vim-ripgrep'
Plug 'tpope/vim-fugitive'
Plug 'vim-utils/vim-man'
Plug 'Valloric/YouCompleteMe'
Plug 'mbbill/undotree'
call plug#end()

"tamaño de la column
set colorcolumn=83

"highlight Colorcolumn ctermbg=220 guibg=lightgrey
colorscheme gruvbox
set background=dark

"CtrlP
set runtimepath^=~/.vim/bundle/ctrlp.vim

"otras configuraciones
if executable('rg')
  let g:rg_derive_root='true'
endif

let g:ctrlp_user_command = ['.git/', 'git --git-dir=%s/.git ls-files -oc --exclude-standard']
let g:netrw_winsize = 25
let g:ctrlp_use_caching = 0

"remapeo de teclas, la tecla para hacer comandos es el espacio
let mapleader = " "
nnoremap <leader>h :wincmd h<CR>
nnoremap <leader>a :wincmd j<CR>
nnoremap <leader>k :wincmd k<CR>
nnoremap <leader>l :wincmd l<CR>
nnoremap <leader>u :UndoTreeShow<CR>
nnoremap <leader>pv :wincmd v<bar> :Ex <bar> :vertical resize 30<CR>
nnoremap <leader>ps :Rg<CR>
nnoremap <silent> <Leader>+ :vertical resize +5<CR>
nnoremap <silent> <leader>- :vertical resize -5<CR>

"Remapeo para definiciones
nnoremap <silent> <Leader>gd :YcmCompleter Goto<CR>
nnoremap <silent> <Leader>gf :YcmCompleter FixIt<CR>
