" Plugin de statusline, la barra inferior
set noshowmode

" Desactivar la tabline
let g:lightline = {
  \ 'enable' : {
    \ 'statusline': 1,
    \ 'tabline': 1
  \},
\ 'colorscheme': 'PaperColor',
\ }

" Algunos temas ya tienen tema para lightline, pero se puede poner
"     especificamente un tema
" let g:lightline = {
"       \ 'colorscheme': 'falcon',
" \ 'colorscheme': 'wal',
"       \ }

" Componentes de cada barra
" Barra inferior
" let g:lightline.active = {
" 		    \ 'left': [ [ 'mode', 'paste' ],
" 		    \           [ 'readonly', 'filename', 'modified' ] ],
" 		    \ 'right': [ [ 'lineinfo' ],
" 		    \            [ 'percent' ],
" 		    \            [ 'fileformat', 'fileencoding', 'filetype' ] ] }
" 		let g:lightline.inactive = {
" 		    \ 'left': [ [ 'filename' ] ],
" 		    \ 'right': [ [ 'lineinfo' ],
" 		    \            [ 'percent' ] ] }
" " Barra superior
" 		let g:lightline.tabline = {
" 		    \ 'left': [ [ 'tabs' ] ],
" 		    \ 'right': [ [ 'close' ] ] }
