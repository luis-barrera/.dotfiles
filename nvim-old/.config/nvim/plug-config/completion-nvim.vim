" Plugin que permite tener recomendaciones para autocompletado mientras
" escribimos o usando la combinación de teclas Control+n, o también Control+p

" Las demás configuraciones se encuentran dentro de la configuración de
" lspconfig, cada uno de los language servers está la config:
" on_attach=require'completion'.on_attach

" Usar completion-nvim en otros buffers, pero usando solo el contenido del
" buffer o snipppets definidos
autocmd BufEnter * lua require'completion'.on_attach()

" Configuración para tener mejor experiencia con el auto completado
set completeopt=menuone,noinsert,noselect

" Quita mensaje cada que se usa el comando de autocompletado
set shortmess+=c

" Soporte para snippets
" TODO se supone que LSP provee snippets por cada lenguaje, pero NeoVim aún no
" los soporta, por lo que debemos usar herramientas como este plugin, en el
" futuro si vuelves a ver esto, revisar el estado de los snippets via LSP en
" neovim
let g:completion_enable_snippet = 'UltiSnips'

" Trigger on delete, recomienda si borramos un caracter
" Muchas personas lo encuetran frutrante
let g:completion_trigger_on_delete = 1

" Podemos tener diversas fuentes de recomendaciones de autocompletado. Para
" moverse entre ellas usamo Control+l
imap <C-l> <Plug>(completion_next_source)

" \{'complete_items': ['lsp', 'snippet', 'buffers', 'ts', 'tabnine']},
let g:completion_chain_complete_list = {
			\'default' : {
			\	'default' : [
			\		{'complete_items' : ['lsp', 'snippet']},
			\		{'mode' : 'file'}
			\	],
			\	'comment' : [],
			\	'string' : []
			\	}
			\}
