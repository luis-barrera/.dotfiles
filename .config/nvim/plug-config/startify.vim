" Plugin que permite ver una pantalla de inicio de sesión cada que se abre nvim
"   también permite guardar la sesión para recuperar los buffers abiertos para recuperar los buffers abiertos
let g:startify_custom_header = [
        \'            ::::    ::::::     ::::::::::::::  :::   :::',
        \'           :+:+:   :+::+:     :+:    :+:     :+:+: :+:+:',
        \'          :+:+:+  +:++:+     +:+    +:+    +:+ +:+:+ +:+',
        \'         +#+ +:+ +#++#+     +:+    +#+    +#+  +:+  +#+',
        \'        +#+  +#+#+# +#+   +#+     +#+    +#+       +#+',
        \'       #+#   #+#+#  #+#+#+#      #+#    #+#       #+#',
        \'      ###    ####    ###    ##############       ###',
        \]

" El plugin permite guardar los buffers abiertos con :SSave y restituirlos con :SLoad
" Aquí está el directorio que guarda las sesiones
let g:startify_session_dir = '~/.config/nvim/session'

" Nombres de las secciones
let g:startify_lists = [
          \ { 'type': 'files',     'header': ['   Recientes']                        },
          \ { 'type': 'sessions',  'header': ['   Sesiones Guardadas']                     },
          \ { 'type': 'bookmarks', 'header': ['   Marcadores']                    },
          \ { 'type': 'dir',       'header': ['   Directorio actual '. getcwd()] },
          \ ]

" Marcadores
let g:startify_bookmarks = [
            \  '~/.config/nvim/init.vim',
            \  '~/.zshrc',
            \  '~/.config/awesome',
            \  '~/scripts',
            \  '~/.config/nvim',
            \ ]

" Muestra iconos
let g:webdevicons_enable_startify = 1
function! StartifyEntryFormat()
        return 'WebDevIconsGetFileTypeSymbol(absolute_path) ." ". entry_path'
endfunction

" Espacios de padding
let g:startify_padding_left = 4
