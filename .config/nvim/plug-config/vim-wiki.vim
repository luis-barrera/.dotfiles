" VimWiki
let g:vimwiki_key_mappings =
    \ {
    \   'all_maps': 1,
    \   'global': 1,
    \   'headers': 1,
    \   'text_objs': 1,
    \   'table_format': 1,
    \   'table_mappings': 1,
    \   'lists': 1,
    \   'links': 1,
    \   'html': 1,
    \   'mouse': 1,
    \ }

" Filetype que puede abrir el plugin
let g:vimwiki_filetypes = ['markdown']

" Directorio donde se encuentra la wiki
let g:vimwiki_list = [{'path': '~/Notas',
                      \ 'syntax': 'markdown', 'ext': '.md', 'exclude_files': ['**/README.md', '**/Readme.md'] }]

let g:vimwiki_diary_months = {
      \ 1: 'January', 2: 'February', 3: 'March',
      \ 4: 'April', 5: 'May', 6: 'June',
      \ 7: 'July', 8: 'August', 9: 'September',
      \ 10: 'October', 11: 'November', 12: 'December'
      \ }

let g:vimwiki_global_ext = 0
