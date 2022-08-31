" set nolinebreak
" set wrapmargin=12
" set wrapmargin=10

" Las lineas largas se muestran como varias lineas
set wrap

" Para hacer fold de código usar zf
setlocal foldmethod=manual

" Lineas que siempre se muestran por encima del cursor
set scrolloff=1

" Continua la linea de comentario al dar enter
" set formatoptions-=o

autocmd FileType vimwiki inoremap <silent><buffer> <CR>
	\ <C-]><Esc>:VimwikiReturn 3 5<CR>
autocmd FileType vimwiki inoremap <silent><buffer> <S-CR>
	\ <Esc>:VimwikiReturn 2 2<CR>
