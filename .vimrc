"           _
"   _   __(_)___ ___  __________
"  | | / / / __ `__ \/ ___/ ___/
" _| |/ / / / / / / / /  / /__
"(_)___/_/_/ /_/ /_/_/   \___/
" Configuración de Vim básica

" Configuraciones indispensables
set nocompatible                " En algunas distros puede haber problema si se tiene instalado varios fork de vi
filetype indent plugin on       " Trata de adivinar el tipo de archivo a partir del nombre
syntax on                       " Resaltado de sintaxis para cada lenguaje
set hidden                      " Mostrar varios editores en una misma ventana, ocultando uno mientras se muestra otro
set wildmenu                    " Mejor autocompletado en la linea de comandos
set showcmd                     " Muestra los comandos que damos
set hlsearch!                   " Resalta los resultados de busqueda
set nomodeline                  " Muestra el modo en el que estamos, por razones de seguridad se recomienda hacer esto
set encoding=utf-8              " Codificación de los char
set iskeyword+=-,_              " Considera las palabras unidas con - y _ como una palabra
set clipboard=unnamedplus       " Copy & paste entre el portapapeles de vim y del sistema
set noswapfile                  " Desactiva el swapfile, en este archivo se guardan copias de seguridad

" Opciones para dar mas usabilidad al editor
set ignorecase                  " Busqueda insensible a las mayusculas
set smartcase                   " Busqueda sensible a las mayúsculas solo si se usan mayúsculas
set incsearch                   " Busca al mismo tiempo que se ingresa la palabra
set backspace=indent,eol,start  " Retroceder en autoindentado, fin de linea o nueva linea
set autoindent                  " Sigue la indentación de la linea anterior
set nostartofline               " Evita que el cursor se ponga sobre el primer char de la linea
set ruler                       " Muestra la posición del cursor
set laststatus=2                " Siempre muestra la linea de estado
set showmode                    " Muestra siempre el modo
set confirm                     " Pide confirmación para guardar el buffer antes de cerrar el editor
set visualbell                  " Advierte visualmente si un problema ocurre
set t_vb=                       " Ayuda a que la advertencia visual no de falsos
set mouse=a                     " Permite usar el mouse en todos los modos
set cmdheight=1                 " Tamaño de la ventana de comandos
set number                      " Número de linea a la izquierda
set relativenumber              " Número de linea respecto a la linea donde esta el cursor
set numberwidth=5               " Tamaño de la columna de numero
set pastetoggle=<F11>           " Cambia entre modo portapapeles y no-portapapeles
set textwidth=130               " No exige una longitud límite de la linea
set linebreak                   " Rompe la linea si se llega al límite de la ventana
set wrap                        " Muestra las linea largas como dos lineas
set title                       " Muestra el nombre del archivo en el titulo de la ventana
set scrolloff=3                 " Muestra 3 lineas más cuando se cuando se navega a través del archivo
set showmatch                   " Muestra en colores los brackets
set tabpagemax=10               " Máximo de tabs
set showtabline=2               " Muestra la barra de tabs siempre
set whichwrap+=<,>,[,],h,l      " Para que el cursor se salte a la linea si estamos en el ultimo char
set splitbelow                  " Si dividimos la ventana horizontalmente, aparece abajo
set splitright                  " Si dividimos la ventana verticalmente, aparece a la derecha
set termguicolors               " No usa los colores que se usan en la terminal
set t_Co=256                    " Soporte para más colores
set t_ut=""                     " Arreglo para color de fondo
set signcolumn=auto             " Muestra una columna con signos, se usa en git y otras cosas
set updatetime=200              " Tasa de refresco del buffer
set colorcolumn=131             " Columna de color mamalona
set undodir=~/.vim/undo-dir     " Directorio donde guarda el archivo de respaldo
set undofile                    " Activa un archivo de respaldo
set cursorline                  " Una linea debajo de la posición del cursor
" Retardo en las teclas
set timeout                     " Retardo en las teclas para acceder una combinación de teclas extra
set ttimeout                    " No retardo en los mapeos de teclas
set ttimeoutlen=100             " Milisegundos de retardo en las teclas
" Tabuladores
set tabstop=8                   " Convierte la tecla tab en 4 espacios
set softtabstop=4
set shiftwidth=4                " Se usan 4 espacios en lugar de tabuladores para indentar
set expandtab
" Status line, en nvim se sobre escribe por airline
set statusline=%F[%m%r%h%w]%=%y[Hex\ %B][Lineas\ %L][%l,%v][%p%%]

" Mapeos
" Tecla leader
let mapleader = "<space>"
" Quitar el resaltado en los resultados de las busquedas
nnoremap <F3> :set hlsearch!<CR>
" Guardar en modo sudo
cmap w!! w !sudo tee % >/dev/null
" Entra al modo normal si pulsamos rápidamente j+k o k+j
inoremap <expr> <c-j> ("\<C-n>")
inoremap <expr> <c-k> ("\<C-p>")
" Copy&paste con las teclas que se usan regularmente, se tiene que instalar el paquete xsel
inoremap <C-v> <ESC>"+pa
vnoremap <C-c> "+y
vnoremap <C-d> "+d
