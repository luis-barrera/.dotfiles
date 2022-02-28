" TODO revisar ortografía de los comentarios
" TODO checar :help autoformat
"    ____      _ __        _
"   /  _/___  (_) /__   __(_)___ ___
"   / // __ \/ / __/ | / / / __ `__ \
" _/ // / / / / /__| |/ / / / / / / /
"/___/_/ /_/_/\__(_)___/_/_/ /_/ /_/
" ===================================

" =============================
" = Configuraciones iniciales =
" =============================
" LeaderKey, usar Space para hacer combinaciones de teclas
let mapleader=' '

" Decirle a NeoVim que deje de intentar ser Vi
set nocompatible
" Portapapeles
set clipboard=unnamedplus

" Trata de adivinar el tipo de archivo
filetype plugin on
" Resaltado de sintaxis para cada lenguaje
syntax enable

" Codificación de los caracteres que podemos usar dentro del editor
set encoding=utf-8
" Considera las palabras unidas con - y _ como una palabra
set iskeyword+=-,_

" Siempre muestra la linea de estado
set laststatus=2
" Muestra siempre el modo en el que estamos
set showmode
" Muestra los comandos que damos
set showcmd
" Tamano de la ventana de comandos
set cmdheight=2
" Pide confirmacion para guardar el buffer antes de cerrar el editor
set confirm

" Tab-Completion cuando buscamos un archivo en el modo comando
" set path+=/home/luisbarrera/,**
set path+=**
" Mejor autocompletado en la línea de comandos
set wildmenu wildmode=longest:full,full

" Búsqueda dentro del archivo
" Búsqueda insensible a las mayúsculas
set ignorecase
" Búsqueda sensible a las mayúsculas solo si se usan mayúsculas
set smartcase
" Busca al mismo tiempo que se ingresa la palabra
set incsearch
" Resalta los resultados de búsqueda
set hlsearch

" Indentado
" Sigue la indentación de la linea anterior
set autoindent
" Retroceder en autoindentado, fin de línea o nueva línea
set backspace=indent,eol,start
" Que el cursor se ponga sobre el primer char de la línea
set nostartofline

" Muestra en colores los brackets
set showmatch
" Advierte visualmente si un problema ocurre
set visualbell
" Ayuda a que la advertencia visual no de falsos
set t_vb=

" Permite usar el mouse en todos los modos
set mouse=a

" Columna de números y signos
" Número de línea a la izquierda
set number
" Número de línea respecto a la línea donde esta el cursor
set relativenumber
" Tamaño de la columna de número
set numberwidth=2
" Muestra una columna con signos, se usa en git y otras cosas
set signcolumn=auto

" Muestra las línea largas como varias líneas
set nowrap
" Para que el cursor se salte a la línea si estamos en el último char
set whichwrap+=<,>,[,],h,l
" Rompe la línea respecto a un espacio y no entre palabras
set linebreak
" Al hacer wrap de una línea, sigue manteniendo el mismo indent
set breakindent

" Mostrar varios editores en una misma ventana, ocultando uno mientras se
"   muestra otro
" set hidden
" Muestra 5 lineas mas cuando se cuando se navega verticalmente archivo
set scrolloff=5
" Muestra el nombre del archivo en el titulo de la ventana
set title
" Máximo de pestanas
" set tabpagemax=10
" Muestra la barra de pestañas abiertas siempre
set showtabline=2

" Dividir ventanas
" Si dividimos la ventana horizontalmente, aparece abajo
set splitbelow
" Si dividimos la ventana verticalmente, aparece a la derecha
set splitright

" Tasa de refresco del buffer
set updatetime=300
" Retardo en las teclas para acceder una combinación de teclas extra
set timeout
" No retardo en los mapeos de teclas
set nottimeout
" Milisegundos de retardo en las teclas
set ttimeoutlen=500

" Agrupar código, con zc y zo
set foldmethod=indent


" ========================
" = Cargar otro archivos =
" ========================
" Snippets personales
source $HOME/.config/nvim/snippets/snippet-config.vim

" Cargar Plugins
source $HOME/.config/nvim/vim-plug/plugins.vim


" =========
" = Temas =
" =========
" Statusline y barra superior
source $HOME/.config/nvim/plug-config/lightline.vim

" Descomentar solo una línea, según el tema que se desee
" source $HOME/.config/nvim/themes/tender.vim
" source $HOME/.config/nvim/themes/material.vim
" source $HOME/.config/nvim/themes/gruvbox.vim
" source $HOME/.config/nvim/themes/spacecamp.vim
" source $HOME/.config/nvim/themes/melange.vim
" source $HOME/.config/nvim/themes/moonfly.vim
" source $HOME/.config/nvim/themes/falcon.vim
" source $HOME/.config/nvim/themes/srcery.vim
source $HOME/.config/nvim/themes/gruvbox-material.vim
" source $HOME/.config/nvim/themes/vim-material.vim

" Para que los temas no tengan problemas con la signcolumn de gitgutter
highlight! link SignColumn LineNr

" Columna de color mamalona
set colorcolumn=80
highlight ColorColumn guifg=#000000 guibg=#BD1E1E

" Resalta palabras similares a las que se encuentra bajo el cursor
" source $HOME/.config/nvim/plug-config/cursorline.vim

" Barra de pestañas mamalona
" source $HOME/.config/nvim/plug-config/bufferline.vim


" =========================
" = Configuración plugins =
" =========================
" LSP nativo, a partir de NeoVim 5.0
source $HOME/.config/nvim/plug-config/nvim-lsp.vim
" Instalar automáticamente server de LSP
" source $HOME/.config/nvim/plug-config/nvim-lspinstall.vim
" Autocompletado
source $HOME/.config/nvim/plug-config/completion-nvim.vim

" Ayuda a identificar cuando usamos f, F, t & T
source $HOME/.config/nvim/plug-config/quickscope.vim

" Comenta y descomenta líneas con gcc
" source $HOME/.config/nvim/plug-config/commentary.vim

" Resalta espacios blancos innecesarios
source $HOME/.config/nvim/plug-config/better-whitespace.vim

" Al correr el comando `:Codi!!` en lenguajes interpretados, se ejecuta código
"   y muestra la salida a la derecha de la pantalla
source $HOME/.config/nvim/plug-config/codi.vim

" Muestra los registros de diff en las columna de colores
source $HOME/.config/nvim/plug-config/gitgutter.vim
" <leader>gm cambios por commit de lo que está debajo el cursor
source $HOME/.config/nvim/plug-config/git-messenger.vim

" Resalta el color de cada entrada del tipo #000000 con su color
source $HOME/.config/nvim/plug-config/nvim-colorizer.vim

" Linea del tiempo de todo el historial de undo con <F6>
source $HOME/.config/nvim/plug-config/undotree.vim

" Goyo, modo sin distracciones
source $HOME/.config/nvim/plug-config/goyo.vim

" NERD-Tree
" source $HOME/.config/nvim/plug-config/NERDTree.vim

" Tree Sitter, parser para varios lenguajes. Aquí también se incluye la
" 	configs de otros plugins como:
" 		- nvim-ts-context-commentstring
" 		- nvim-ts-rainbow
source $HOME/.config/nvim/plug-config/treesitter.vim

" Telescope, fuzzy finder
source $HOME/.config/nvim/plug-config/telescope.vim

" Encuentra y muestra posibles errores en el código
" source $HOME/.config/nvim/plug-config/trouble.vim

" Wiki personal, archivos .md
source $HOME/.config/nvim/plug-config/vim-wiki.vim

" Todo-comments, con los comandos TodoQuickFix, TodoTruble y TodoTelescope
" 	podemos ver aquellos comentarios con palabras TODO, PERF, HACK, NOTE, FIX
" 	y WARNING dentro del dir de un proyecto
" source $HOME/.config/nvim/plug-config/todo-comments.vim

" Auto-Sesión, NVim tiene incluido un administrador de sesiones, si se abre
" 	una ventana de nvim sin argumentos dentro de un directorio, entonces nvim
" 	tratará de abrir los mismos buffers que estaban abiertos en la última vez
" 	que abrimos nvim en ese directorio.
" source $HOME/.config/nvim/plug-config/auto-session.vim

" Muestra posibles recomendaciones para resolver el problema que hay bajo el
" 	cursor, para mostrar las posibles soluciones, usar :Telescope
" 	lsp-code-actions
" source $HOME/.config/nvim/plug-config/nvim-lightbulb.vim

" Nabla, renderizar ecuaciones LaTeX usando ASCII, muy inestable por el
" 	momento
" source $HOME/.config/nvim/plug-config/nabla.vim

" revJ hacer lo contrario a J, en lugar de unir líneas las separa, sirve para
" 	separar argumentos o la especificación de parámetros de una función en
" 	varias líneas, funciona seleccionando en modo VISUAL y luego <leader>-j
" source $HOME/.config/nvim/plug-config/revj.vim

" Neuron.vim, neuron es una aplicación para administrar nuestras notas usando
" 	el sistema Zettelkasten, un sistema muy eficiente para tomar y organizar
" 	nuestras notas. Este plugin permite que dentro de un directorio podamos
" 	tener varios archivos .md que se integran con este sistema.
" source $HOME/.config/nvim/plug-config/neuron.vim

" Líneas verticales que muestran mejor los niveles de indentación en un bloque
" 	de código
" source $HOME/.config/nvim/plug-config/indent-blankline.vim

" Oscurece ventanas que no están activas cuando hay más de una ventana en el
" 	editor
" source $HOME/.config/nvim/plug-config/shade.vim

" LSP-Saga
" source $HOME/.config/nvim/plug-config/lspsaga.vim

" Muestra las combinaciones de teclas de la barra esp.
" source $HOME/.config/nvim/keys/which-key.vim

" Pegar imágenes desde el clipboard
" source $HOME/.config/nvim/plug-config/clipboard-image.vim

" vimtex, LaTeX en nvim
let g:vimtex_compiler_progname = 'nvr'

" LiveDown, preview de markdown
let g:livedown_browser = "surf" " Navegador a usar

" ========
" = Tabs =
" ========
" Establecer tabstop, softtabstop y shiftwidth al mismo nivel
source $HOME/.config/nvim/functions/stab.vim
source $HOME/.config/nvim/functions/toggleindent.vim
" Convierte los tabs en espacios
set noexpandtab
set copyindent
set preserveindent
" Convierte la tecla tab en 8 espacios
set tabstop=4
" Detecta mejor si varios espacios son un TAB al momento de borrar
" set softtabstop=0
" Se usan 2 espacios en lugar de tabuladores para indentar
set shiftwidth=4
" Al insertar un TAB después de texto, inserta los 2 espacios
" set smarttab
set listchars+=tab:│•
" set listchars+=eol:¶
" Muestra los tabuladores con un caracter > o el que pongamos con listchars
set list
" Para convertir tabs en espacios en documentos que tienen tabs, usar :retab


" =============
" = Funciones =
" =============
" Sustituir la linea actual con la salida del comando `date`
source $HOME/.config/nvim/functions/getFecha.vim
" Limpiar todos los registros
source $HOME/.config/nvim/functions/clearRegisters.vim


" ====================
" = Mapeos de teclas =
" ====================
" Algunos plugins tienen también sus propios mappings
source $HOME/.config/nvim/keys/mappings.vim


" ============
" = Reconfig =
" ============
" Al parecer hay plugins que sobreescriben las coniguraciones, por eso las
" cambio de lugar. Antes estaban al principio, ahora al final
" Directorio donde guardar el registro de cambios de un archivo
set undodir=$HOME/.config/nvim/undodir

" Desactiva el swapfile, en este archivo se guardan copias de seguridad
set noswapfile
" Activa un archivo de respaldo
set undofile
" No crea archivos de backup
set nobackup
set nowritebackup

" Desactiva que se inserte una línea de comentario si hacemos ^O o ^o en una
"   linea comentada, es un comportamiento que, en lo personal, me disgusta
set formatoptions-=o
autocmd FileType * set formatoptions-=o
