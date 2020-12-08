
set nocompatible

augroup lexical
  autocmd!
  autocmd FileType markdown,mkd call lexical#init()
  autocmd FileType textile call lexical#init()
  autocmd FileType text call lexical#init({ 'spell': 0 })
  autocmd FileType tex call lexical#init()
augroup END

let g:lexical#spelllang = ['es_MX']
