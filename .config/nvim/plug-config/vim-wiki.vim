" VimWiki
" Agrupar listas con zM y zr en Normal-Mode
" let g:vimwiki_folding='list'

" Muestra la extensión de los archivos MarkDown
let g:vimwiki_markdown_link_ext=1

" Filetype que puede abrir el plugin
let g:vimwiki_filetypes = ['markdown']

" Directorio donde se encuentra la wiki
let g:vimwiki_list = [{'path': '~/Notas',
						\ 'syntax': 'markdown',
						\ 'ext': '.md',
						\ 'exclude_files': ['**/README.md', '**/Readme.md'] }]


let g:vimwiki_url_maxsave=0

let g:vimwiki_folding='custom'

" let vimwiki_ext2syntax = {}
