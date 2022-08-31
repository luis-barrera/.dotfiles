" Plugin que muestra los registros de git diff en la columna de la derecha

" Activar el plugin
let g:gitgutter_enabled=1

" Resalta las líneas que tienen cambios
" let g:gitgutter_highlight_lines=1

" Signos que usa el plugin para señalar los cambios
let g:gitgutter_sign_added              = '+'
let g:gitgutter_sign_modified           = '~'
let g:gitgutter_sign_removed            = '_'
let g:gitgutter_sign_removed_first_line = '‾'
let g:gitgutter_sign_removed_above_and_below = '_¯'
let g:gitgutter_sign_modified_removed   = '~_'

" Muestra los cambios en una ventana flotante
let g:gitgutter_preview_win_floating=1

" Colores que se muestran en la columna de la izquierda
highlight GitGutterAdd    guifg=#587C0C ctermfg=2
highlight GitGutterChange guifg=#0C7D9D ctermfg=3
highlight GitGutterDelete guifg=#94151B ctermfg=1
