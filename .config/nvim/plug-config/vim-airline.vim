" Para usar funtes especiales de powerline
let g:airline_powerline_fonts = 1

" Habilita la tab superior
let g:airline#extensions#tabline#enabled = 1

" Formato en el que muestran los nombres de los archivos en la tab superior
let g:airline#extensions#tabline#formatter = 'jsformatter'

" Información a presentar
let g:airline#extensions#default#layout = [['a', 'b', 'c'], ['x', 'z', 'warning', 'error']]
