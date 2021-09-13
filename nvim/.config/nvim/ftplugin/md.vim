" set textwidth
" set nolinebreak
" set wrapmargin=10
" set wrap

" Desactiva que se inserte una línea de comentario si hacemos ^O o ^o en una
"   linea comentada, es un comportamiento que, en lo personal, me disgusta
set formatoptions-=o

autocmd FileType vimwiki inoremap <silent><buffer> <CR>
	\ <C-]><Esc>:VimwikiReturn 3 5<CR>
autocmd FileType vimwiki inoremap <silent><buffer> <S-CR>
	\ <Esc>:VimwikiReturn 2 2<CR>

set omnifunc-=compe#complete()
