" Configuración para usar snippets
" La parte importante aquí, es el comando `read`, que nos permite pegar el
"   contenido de un archivo dentro del buffer que tenemos abierto

" Inserta esqueleto básico de HTML
command! SnipHTML execute "-1read $HOME/.config/nvim/snippets/skeleton.html"

" Inserta un esqueleto básico de C Lang
command! SnipCprog execute "-1read $HOME/.config/nvim/snippets/skeleton.c"
