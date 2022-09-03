" LeaderKey, usar Space para hacer combinaciones de teclas
let mapleader=' '

" Navegación entre ventanas
nnoremap <C-h> <ESC><C-w>h
nnoremap <C-j> <ESC><C-w>j
nnoremap <C-k> <ESC><C-w>k
nnoremap <C-l> <ESC><C-w>l

" Resize de las ventanas
nnoremap <M-j> 1<C-w>+
nnoremap <M-k> 1<C-w>-
nnoremap <M-h> 1<C-w><
nnoremap <M-l> 1<C-w>>

" Entrar a modo normal usando la combinacion j-k o k-j
inoremap jk <Esc>
inoremap kj <Esc>
nnoremap <silent> <C-c> <Esc>

" Navegar entre las recomendaciones de autocompletado
" inoremap <expr> <Tab> ("\<C-n>")
" inoremap <expr> <S-Tab> ("\<C-p>")

" Indentar en NORMAL-mode
vnoremap < <gv
vnoremap > >gv

" Moverse entre las pestañas abiertas
nmap <Tab> :tabnext<CR>
nmap <S-Tab> :tabprev<CR>

" Mover bloque selecionado en VISUAL-mode arriba o abajo
xnoremap <S-k> :move '<-2<CR>gv-gv
xnoremap <S-j> :move '>+1<CR>gv-gv

" Alternativas a guardar y salir
nnoremap <silent> <C-s> :w<CR>
nnoremap <silent> <C-Q> :wq!<CR>

" Mejor movimiento entre lineas recortadas por el largo de ventana
nnoremap <silent> j gj
nnoremap <silent> k gk

" Quitar el resaltado en los resultados de las busquedas
nnoremap <F9> :set hlsearch!<CR>

" Borrar hacia atras
" imap <C-H> <C-W>
" noremap! <C-H> <C-W>

map <silent> cf :tabe <cfile><cr>
map <silent> gf :tabe <cfile><cr>
