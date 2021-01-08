" Plugin que ejecuta codigo de lenguajes interpretados al momento de escribirlos
"   y los muestra en la parte derecha la salida (funciona con py, js, haskell, R, Lua, C++ y más)
" Para activarlo, correr :Codi!!

" Color de la salida
highlight CodiVirtualText guifg='#98C379'
" Primer caracter de la salida
let g:codi#virtual_text_prefix = "❯ "
