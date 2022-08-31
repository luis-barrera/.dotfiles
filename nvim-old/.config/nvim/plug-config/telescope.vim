" Telescope permite buscar cualquier cosa dentro de neovim
" Para esto, utilizan varios pickers que se encargan de encontrar ciertos
" elementos (archivos, lineas dentro del código, palabras dentro de un
" archivo, símbolos o funciones, etc).
" Para saber cuales están disponibles y poder asignarlos a keymap, visitar su
" página de GitHub

" Encontras archivos
nnoremap <leader>ff <cmd>Telescope find_files<cr>

" Encontrar palabras dentro de archivos
nnoremap <leader>fg <cmd>Telescope live_grep<cr>

" Encontrar dentro de los buffers abiertos
nnoremap <leader>fb <cmd>Telescope buffers<cr>

" Encontrar la palabra bajo el cursor en otros archivos
nnoremap <leader>fw <cmd>Telescope grep_string<cr>

" Abrir un explorador de archivos
nnoremap <leader>fd <cmd>Telescope file_browser<cr>

" Muestra el contenido de los registros (portapeles) internos de neovim
nnoremap <leader>fr <cmd>Telescope registers<cr>

" Muestra el contenido de TreeSitter: nombre de funciones y variables
nnoremap <leader>ft <cmd>Telescope treesitter<cr>

" Encontrar archivos de manual o de ayuda
nnoremap <leader>fh <cmd>Telescope help_tags<cr>

" Mostrar una lista de todos los pickers
nnoremap <leader>f <cmd>Telescope builtin<cr>
