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
" VimWiki
  Plug 'vimwiki/vimwiki'

" LSP Nativo de NVim
	Plug 'neovim/nvim-lspconfig'
" Instalar automáticamente los server de lenguajes para lsp
	" Plug 'kabouzeid/nvim-lspinstall'
" Para mejorar los colorschemes usando LSP
	Plug 'folke/lsp-colors.nvim'

" Autocompletado con LSP
	Plug 'haorenW1025/completion-nvim'
" Complemento para completion-nvim, usa treessitter para dar recomendaciones
	Plug 'nvim-treesitter/completion-treesitter'
" Complemento para completion-nvim, usa los buffers para dar recomendaciones
	Plug 'steelsojka/completion-buffers'
" Complemento para completion-nvim, usa los ctags para dar recomendaciones
	Plug 'kristijanhusak/completion-tags'
" Complemento para completion-nvim, usa snippets para dar recomendaciones
	Plug 'SirVer/ultisnips'
	Plug 'honza/vim-snippets'
" Complemento para completion-nvim, usa tabnine para dar recomendaciones
	Plug 'aca/completion-tabnine', { 'do': './install.sh' }

" Tree Sitter, analizador de parser para lenguajes
	Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
	Plug 'nvim-treesitter/playground'

" Paréntesis de colores, requiere de TreeSitter
	Plug 'p00f/nvim-ts-rainbow'

" Mejores comentarios con el comando gcc, requiere de TreeSitter
	Plug 'JoosepAlviste/nvim-ts-context-commentstring'

" Nvim Devicons
	Plug 'kyazdani42/nvim-web-devicons'
	Plug 'folke/trouble.nvim'

" Fuzzy Finder
	Plug 'nvim-lua/popup.nvim'
	Plug 'nvim-lua/plenary.nvim'
	Plug 'nvim-telescope/telescope.nvim'

" Todo-comments, lista cosas por hacer en cada código, requiere plenary.nvim
	Plug 'folke/todo-comments.nvim'

" Cuando hay varias ventanas abiertas, oscurece las que no están enfocadas
	Plug 'sunjon/shade.nvim'

" Resalta palabras iguales a la que se encuentra bajo el cursor
	" Plug 'yamatsum/nvim-cursorline'

" Explorador de archivos
	Plug 'preservim/nerdtree'

" Auto-Sesión
	" Plug 'rmagatti/auto-session'

" Plugin que te da recomendaciones para resolver algún problema
	Plug 'kosayoda/nvim-lightbulb'

" Mostrar ecuaciones LaTeX dentro de Vim usando ASCII
	Plug 'jbyuki/nabla.nvim'

" Hacer lo contrario a J, en lugar de unir líneas las separa
	Plug 'kana/vim-textobj-user'
	Plug 'sgur/vim-textobj-parameter'
	Plug 'AckslD/nvim-revJ.lua'

" Tomar notas de forma más eficiente y ordenada usando el método Zettelkasten
	" Plug 'oberblastmeister/neuron.nvim', {'branch': 'unstable'}
	" Plug 'nvim-lua/popup.nvim'
	" Plug 'nvim-lua/plenary.nvim'
	" Plug 'nvim-telescope/telescope.nvim'

" Pegar imágenes desde el clipboard
	Plug 'ekickx/clipboard-image.nvim'

" Mostrar los símbolos del proyecto en una vista a la derecha con el comando
" :SymbolsOutline y dentro del menú con r podemos renombrarlo
	Plug 'simrat39/symbols-outline.nvim'

" Plugin para incrementar más fácil fechas con C-a y C-x
	Plug 'tpope/vim-speeddating'

" Plugin para ver el contenido de los diferentes registros de nvim, los
" registros son el portapapeles interno de nvim. Con "" activamos este plugin.
	Plug 'gennaro-tedesco/nvim-peekup'

" Preview de markdown
	Plug 'shime/vim-livedown'

" Mostrar el contexto al final de una función, sirve para identificar varios
" más facilmente las funciones en código muy grande.
	Plug 'haringsrob/nvim_context_vt'

"" TODO

" Mejor representación de las lineas indentadas a través de líneas verticales
" que marcan los bloques de código
	" Plug 'lukas-reineke/indent-blankline.nvim'


  " Cierra los parentesis, llaves y corchetes
  Plug 'jiangmiao/auto-pairs'

  " Multi-cursor
  " Plug 'mg979/vim-visual-multi', {'branch': 'master'}

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
  " Plug 'sheerun/vim-polyglot'

  " Sopore para comandos Git en nvim
  " Plug 'tpope/vim-fugitive'

  " Muestra información de los commits con <leader>gm
  " Plug 'rhysd/git-messenger.vim'

  " Soporte para Github en nvim
  " Plug 'tpope/vim-rhubarb'

  " Muestra el historial de commits con :GV o :GV!
  " Plug 'junegunn/gv.vim'

  " Snippets para webdev
  " Plug 'mattn/emmet-vim'

  " Comentarios con el keymap gcc
  Plug 'tpope/vim-commentary'

  " Página de inico
  " Plug 'mhinz/vim-startify'

  " Muestra espacios en blanco innecesarios
  Plug 'ntpeters/vim-better-whitespace'

  " Muestra los keymaps presionando <space>
  " Plug 'liuchengxu/vim-which-key'

  " Muestra salida del codigo mientras se escribe
  Plug 'metakirby5/codi.vim'

  " Muestra los colores #ffffff
  Plug 'norcalli/nvim-colorizer.lua'

  " Resalta posibles movimientos con f, F, t, T
  Plug 'unblevable/quick-scope'

  " Paréntesis de colores
  " Plug 'kien/rainbow_parentheses.vim'

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

  " Netrw mejorado, con , abrimos el explorador, con <C-6> volvemos al buffer
  Plug 'tpope/vim-vinegar'

  " Detectar mejor las palabras al usar w, e & b
  " Plug 'chaoren/vim-wordmotion'

" END TODO

" Temas
	Plug 'kaicataldo/material.vim'
	Plug 'morhetz/gruvbox'
	Plug 'bluz71/vim-moonfly-colors'
	Plug 'savq/melange'
	Plug 'jaredgorski/spacecamp'
	Plug 'jacoborus/tender.vim'
	Plug 'fenetikm/falcon'
	Plug 'srcery-colors/srcery-vim'
	Plug 'sainnhe/gruvbox-material'
	Plug 'hzchirs/vim-material'

" Statusline
	Plug 'itchyny/lightline.vim'

" Pywal en nvim
	Plug 'dylanaraps/wal.vim'

" Pestañas más bonitas
	" Plug 'akinsho/nvim-bufferline.lua'

call plug#end()

" Automatically install missing plugins on startup
autocmd VimEnter *
	\  if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
	\|   PlugInstall --sync | q
	\| endif
