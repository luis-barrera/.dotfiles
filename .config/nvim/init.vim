"    ____      _ __        _
"   /  _/___  (_) /__   __(_)___ ___
"   / // __ \/ / __/ | / / / __ `__ \
" _/ // / / / / /__| |/ / / / / / / /
"/___/_/ /_/_/\__(_)___/_/_/ /_/ /_/

" Exporta la configuración de Vim a NeoVim
source $HOME/.vimrc

" Sobreescribir configuración de undodir
set undodir=$HOME/.config/nvim/undodir

" General Settings
" ----------------
" Mapeos de teclas
source $HOME/.config/nvim/keys/mappings.vim

" Snippets personales
source $HOME/.config/nvim/snippets/snippet-config.vim

" Cargar Plugins
source $HOME/.config/nvim/vim-plug/plugins.vim

" Configuración de algunos plugins, algunos otros no necesitan configuración
" --------------------------------------------------------------------------
" LSP nativo, a partir de NeoVim 5.0
source $HOME/.config/nvim/plug-config/nvim-lsp.vim
luafile $HOME/.config/nvim/plug-config/compe.lua
source $HOME/.config/nvim/plug-config/vsnip.vim






" Ayuda a identificar cuando usamos f, F, t & T
source $HOME/.config/nvim/plug-config/quickscope.vim
" Wiki personal, archivos .md
source $HOME/.config/nvim/plug-config/vim-wiki.vim
" Comenta y descomenta líneas con gcc
source $HOME/.config/nvim/plug-config/vim-commentary.vim
" Con el comando :LLP muestra la salida de un archivo LaTeX
source $HOME/.config/nvim/plug-config/latex-live-preview.vim
" Resalta en #111111 espacios blancos innecesarios
source $HOME/.config/nvim/plug-config/better-whitespace.vim
" Ejecuta código python y muestra la salida a la derecha
source $HOME/.config/nvim/plug-config/codi.vim
" Pantalla de inicio, también permite guardar sesiones
" source $HOME/.config/nvim/plug-config/startify.vim
" Muestra los registros de diff en las columna de colores
source $HOME/.config/nvim/plug-config/gitgutter.vim
" <leader>gm cambios por commit de lo que está debajo el cursor
source $HOME/.config/nvim/plug-config/git-messenger.vim
" Cierra automáticamente tags de html
source $HOME/.config/nvim/plug-config/closetags.vim
" Si hacemos cambios a una tag abierta, se repite en la cerrada
source $HOME/.config/nvim/plug-config/tagalong.vim
" Colores para cada par de parentesis, brackets y llaves
source $HOME/.config/nvim/plug-config/rainbow_parentheses.vim
" Resalta el color de cada entrada del tipo #XXXXXX con su color
source $HOME/.config/nvim/plug-config/nvim-colorizer.vim
" Linea del tiempo de todo el historial de undo
source $HOME/.config/nvim/plug-config/undotree.vim
" Snippet Snippets
" source $HOME/.config/nvim/plug-config/snipmate.vim
" Statusline
source $HOME/.config/nvim/plug-config/lightline.vim
" Goyo
source $HOME/.config/nvim/plug-config/goyo.vim
" Muestra las combinaciones de teclas de la barra esp.
source $HOME/.config/nvim/keys/which-key.vim

" Temas
" source $HOME/.config/nvim/themes/material.vim
source $HOME/.config/nvim/themes/gruvbox.vim
" source $HOME/.config/nvim/themes/spacecamp.vim
" source $HOME/.config/nvim/themes/melange.vim
" source $HOME/.config/nvim/themes/moonfly.vim

" Convierte la tecla tab en 4 espacios
set tabstop=8
" Detecta mejor si varios espacios son un TAB al momento de borrar
set softtabstop=2
" Se usan 2 espacios en lugar de tabuladores para indentar
set shiftwidth=2
" Convierte los tabs en espacios
set noexpandtab
" Para convertir tabs en espacios en documentos que tinen tabs, usar :retab

" Unmaps, a pesar de usar noremap, hay sobreescritura de keymaps
source $HOME/.config/nvim/keys/unmappings.vim
