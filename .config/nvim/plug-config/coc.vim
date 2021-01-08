" Plugin para autocompletado
"  Lenguajes a mostrar autocompletado
let g:coc_global_extensions = [
    \ 'coc-clangd',
    \ 'coc-css',
    \ 'coc-explorer',
    \ 'coc-gist',
    \ 'coc-git',
    \ 'coc-go',
    \ 'coc-html',
    \ 'coc-java',
    \ 'coc-json',
    \ 'coc-markdownlint',
    \ 'coc-snippets',
    \ 'coc-actions',
    \ 'coc-sh',
    \ 'coc-java-debug',
    \ 'coc-java',
    \ 'coc-python',
    \ 'coc-rust-analyzer',
    \ 'coc-sh',
    \ 'coc-snippets',
    \ 'coc-svg',
    \ 'coc-tabnine',
    \ 'coc-texlab',
    \ 'coc-todolist',
    \ 'coc-marketplace',
    \ ]

" Con <tab> se lanza el menú de autocompletado
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Usa <c-space> para mostrar el autocompletado
inoremap <silent><expr> <c-space> coc#refresh()

" Con <cr> acepta el autocompletado, `<C-g>u` means break undo chain at current
if exists('*complete_info')
  inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
else
  imap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endif

" GoTo Navegación.
" Mostrar la definición de un método
nmap <silent> gd <Plug>(coc-definition)
" Muestra los tipos en la definición de un método
nmap <silent> gy <Plug>(coc-type-definition)
" Muestra la implementación de un método
nmap <silent> gi <Plug>(coc-implementation)
" Muestra las referencias a ese método
nmap <silent> gr <Plug>(coc-references)
" Con K se muestra la docmentación sobre una palabra
nnoremap <silent> K :call <SID>show_documentation()<CR>
function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Muestra otra presencia de la palabra que está debajo el cursor
autocmd CursorHold * silent call CocActionAsync('highlight')

" Para renombrar en grupo toda la presencia de un método o variable
nmap <leader>rn <Plug>(coc-rename)

" :Format Comando para darle formato al buffer
command! -nargs=0 Format :call CocAction('format')

" :Fold comando para agrupar hacer fold del código
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" :OR comando para obtener las librerias necesarias
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Soporte para statusline
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Explorador de archvos con <space>e
let g:coc_explorer_global_presets = {
\   'floating': {
\      'position': 'floating',
\   },
\   'floatingLeftside': {
\      'position': 'floating',
\      'floating-position': 'left-center',
\      'floating-width': 40,
\   },
\   'floatingRightside': {
\      'position': 'floating',
\      'floating-position': 'right-center',
\      'floating-width': 30,
\   },
\   'simplify': {
\     'file.child.template': '[selection | clip | 1] [indent][icon | 1] [filename omitCenter 1]'
\   }
\ }
" nmap <silent> <space>e :CocCommand explorer<CR>
nnoremap <silent> <leader>e :CocCommand explorer<CR>
" nmap <space>f :CocCommand explorer --preset floatingRightside<CR>
autocmd BufEnter * if (winnr("$") == 1 && &filetype == 'coc-explorer') | q | endif

" Snippets
" Use <C-l> for trigger snippet expand.
imap <C-l> <Plug>(coc-snippets-expand)
" Use <C-j> for select text for visual placeholder of snippet.
vmap <C-j> <Plug>(coc-snippets-select)
" Use <C-j> for jump to next placeholder, it's default of coc.nvim
let g:coc_snippet_next = '<c-j>'
" Use <C-k> for jump to previous placeholder, it's default of coc.nvim
let g:coc_snippet_prev = '<c-k>'
" Use <C-j> for both expand and jump (make expand higher priority.)
imap <C-j> <Plug>(coc-snippets-expand-jump)
