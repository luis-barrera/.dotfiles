"   _   __(_)___ ___  __________
"  | | / / / __ `__ \/ ___/ ___/
" _| |/ / / / / / / / /  / /__
"(_)___/_/_/ /_/ /_/_/   \___/
" Configuracion de VIM basica
" Deliberadamente no uso tildes en este archivo

" Decirle a Vim que deje de intentar ser Vi
set nocompatible

" Trata de adivinar el tipo de archivo a partir del nombre
filetype plugin on
" Resaltado de sintaxis para cada lenguaje
syntax enable

" Tab-Completion cuando buscamos un archivo en el modo comando
set path+=**
" Mejor autocompletado en la linea de comandos
set wildmenu

" Copy & paste entre el portapapeles de vim y del sistema
set clipboard=unnamedplus
" Cambia entre modo portapapeles y no-portapapeles
set pastetoggle=<F11>

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

" Codificacion de los caracteres que podemos usar
set encoding=utf-8
" Considera las palabras unidas con - y _ como una palabra
set iskeyword+=-,_

" Desactiva el swapfile, en este archivo se guardan copias de seguridad
set noswapfile                  
" Directorio donde guarda el archivo de respaldo
set undodir=~/.vim/undo-dir
" Activa un archivo de respaldo
set undofile
" No crea archivos de backup
set nobackup
set nowritebackup

" netrw es un plugin built-in de vim, cuando corremos `:edit .` nos abre un
"   navegador de archivos en el directorio actual
let g:netrw_banner=0            " Deshabilita el banner
let g:netrw_browse_split=4      " Abre una ventana nueva
let g:netrw_altv=1              " Abre los archivos a la derecha
let g:netrw_liststyle=3         " Tree view

" Busqueda insensible a las mayusculas
set ignorecase
" Busqueda sensible a las mayusculas solo si se usan mayusculas
set smartcase
" Busca al mismo tiempo que se ingresa la palabra
set incsearch
" Resalta los resultados de busqueda
set hlsearch!

" Retroceder en autoindentado, fin de linea o nueva linea
set backspace=indent,eol,start
" Sigue la indentacion de la linea anterior
set autoindent
" Que el cursor se ponga sobre el primer char de la linea
set nostartofline

" Columna de color mamalona
set colorcolumn=80
highlight ColorColumn guifg=#000000 guibg=#5F9EA0

" Muestra en colores los brackets
set showmatch
" Advierte visualmente si un problema ocurre
set visualbell
" Ayuda a que la advertencia visual no de falsos
set t_vb=

" Permite usar el mouse en todos los modos
set mouse=a

" Numero de linea a la izquierda
set number
" Numero de linea respecto a la linea donde esta el cursor
set relativenumber
" Tamano de la columna de numero
set numberwidth=5
" Muestra una columna con signos, se usa en git y otras cosas
set signcolumn=yes

" Muestra las linea largas como varias lineas
set wrap
" Para que el cursor se salte a la linea si estamos en el ultimo char
set whichwrap+=<,>,[,],h,l
" Rompe la linea respecto a un espacio y no entre palabras
set linebreak
" Al hacer wrap de una linea, sigue manteniendo el mismo indent
set breakindent

" Mostrar varios editores en una misma ventana, ocultando uno mientras se
"   muestra otro
set hidden
" Muestra 3 lineas mas cuando se cuando se navega a traves del archivo
set scrolloff=3
" Muestra el nombre del archivo en el titulo de la ventana
set title
" Maximo de pestanas
set tabpagemax=6
" Muestra la barra de pestanas abiertas siempre
set showtabline=2

" Si dividimos la ventana horizontalmente, aparece abajo
set splitbelow
" Si dividimos la ventana verticalmente, aparece a la derecha
set splitright

" Tasa de refresco del buffer
set updatetime=300
" Retardo en las teclas para acceder una combinacion de teclas extra
set timeout
" No retardo en los mapeos de teclas
set ttimeout
" Milisegundos de retardo en las teclas
set ttimeoutlen=100

" Convierte la tecla tab en 4 espacios
set tabstop=8
" Detecta mejor si varios espacios son un TAB al momento de borrar
set softtabstop=2
" Se usan 2 espacios en lugar de tabuladores para indentar
set shiftwidth=2
" Convierte los tabs en espacios
set noexpandtab
" Para convertir tabs en espacios en documentos que tinen tabs, usar :retab

" Mapeos y comandos
" ------
" Quitar el resaltado en los resultados de las busquedas
nnoremap <F3> :set hlsearch!<CR>

" Tags, navegacion entre las partes del codigo
command! MakeTags !ctags -R .


" Colores
" -------
set t_Co=256                    " Soporte para mas colores
set t_ut=""                     " Solucion para color de fondo
set termguicolors               " No usa los colores que se usan en la terminal
" colorscheme industry

" Status line, en nvim se sobre escribe por statusline
set statusline=%F[%m%r%h%w]%=%y[Hex\ %B][Lineas\ %L][%l,%v][%p%%]
