"    ____      _ __        _
"   /  _/___  (_) /__   __(_)___ ___
"   / // __ \/ / __/ | / / / __ `__ \
" _/ // / / / / /__| |/ / / / / / / /
"/___/_/ /_/_/\__(_)___/_/_/ /_/ /_/

" General Settings
source $HOME/.config/nvim/vim-plug/plugins.vim                  " Plugins
source $HOME/.vimrc                                             " Exporta la config de vim
source $HOME/.config/nvim/keys/mappings.vim                     " Mapeos de teclas

" Configuración de algunos plugins, hay algunos plugin que no necesitan configuración
source $HOME/.config/nvim/plug-config/quickscope.vim            " Ayuda a identificar cuando usamos f, F, t & T
source $HOME/.config/nvim/plug-config/vim-commentary.vim        " Comenta y descomenta líneas con gcc
source $HOME/.config/nvim/plug-config/latex-live-preview.vim    " Con el comando :LLP muestra la salida de un archivo LaTeX
source $HOME/.config/nvim/plug-config/better-whitespace.vim     " Resalta en #111111 espacios blancos innecesarios
source $HOME/.config/nvim/plug-config/codi.vim                  " Ejecuta código python y muestra la salida a la derecha
source $HOME/.config/nvim/plug-config/startify.vim              " Pantalla de inicio, también permite guardar sesiones
source $HOME/.config/nvim/plug-config/vim-wiki.vim              " Wiki personal, archivos .md
source $HOME/.config/nvim/plug-config/coc.vim                   " Autocompletado
source $HOME/.config/nvim/plug-config/gitgutter.vim             " Muestra los registros de diff en las columna de colores
source $HOME/.config/nvim/plug-config/git-messenger.vim         " <leader>gm cambios por commit de lo que está debajo el cursor
source $HOME/.config/nvim/plug-config/closetags.vim             " Cierra automáticamente tags de html
source $HOME/.config/nvim/plug-config/tagalong.vim              " Si hacemos cambios a una tag abierta, se repite en la cerrada
source $HOME/.config/nvim/plug-config/rainbow_parentheses.vim   " Colores para cada par de parentesis, brackets y llaves
source $HOME/.config/nvim/plug-config/nvim-colorizer.vim        " Resalta el color de cada entrada del tipo #XXXXXX con su color
source $HOME/.config/nvim/plug-config/undotree.vim              " Linea del tiempo de todo el historial de undo
source $HOME/.config/nvim/plug-config/pencil.vim                " Wrap para archivos markdown
source $HOME/.config/nvim/plug-config/lightline.vim             " Statusline
source $HOME/.config/nvim/keys/which-key.vim                    " Muestra las combinaciones de teclas de la barra esp.

" Temas
"source $HOME/.config/nvim/themes/material.vim
source $HOME/.config/nvim/themes/gruvbox.vim
