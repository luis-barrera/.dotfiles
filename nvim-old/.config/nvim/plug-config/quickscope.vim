" Ayuda a detectar los posibles movimientos que podemos hacer con f, F, t, T
let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']

" Siempre activado
let g:qs_enable=1

" Máximo de caracteres a tomar en cuenta
let g:qs_max_chars=1000

" Tiempo necesario para que se active el plugin
let g:qs_delay=100

" Colores especiales
highlight QuickScopePrimary guifg='#6664DF' gui=undercurl ctermfg=155 cterm=undercurl
highlight QuickScopeSecondary guifg='#eF5F70' gui=undercurl ctermfg=81 cterm=undercurl
