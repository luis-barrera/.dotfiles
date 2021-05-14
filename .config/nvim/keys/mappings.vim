" Navegación entre ventanas
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Resize de las ventanas
nnoremap <silent> <M-h> :vertical resize +1<CR>
nnoremap <silent> <M-l> :vertical resize -1<CR>
nnoremap <silent> <M-j> :resize -1<CR>
nnoremap <silent> <M-k> :resize +1<CR>

" Entrar a modo normal usando la combinacion j-k o k-j
inoremap jk <Esc>
inoremap kj <Esc>
nnoremap <silent> <C-c> <Esc>

" Navegar entre las recomendaciones de autocompletado
inoremap <expr> <C-j> ("\<C-n>")
inoremap <expr> <C-k> ("\<C-p>")

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
inoremap <silent> j <C-o>gj
inoremap <silent> k <C-o>gk
nnoremap <silent> j gj
nnoremap <silent> k gk

" Leader Key, usar Space para hacer combinaciones de teclas
let mapleader=" "
