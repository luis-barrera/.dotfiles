" Lista de servers del native nvim language server protocol
"	 los nombres de los servers disponibles se encuentran en:
"	 https://github.com/neovim/nvim-lspconfig/blob/master/CONFIG.md
" Cada uno de estos servers deben ser instalados por separado. Visitar la
"	 página anterior para más información.

" Característica necesaria para mostrar la ventana de sugerencias
" set omnifunc=compe#complete()

" Inicia bloque de código de Lua, aquí se declaran los lenguajes y sus
"		respectivos servidores a usar en el LSP nativo
lua << EOF
-- Función que permite abrir el popup de recomendaciones
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
capabilities.textDocument.completion.completionItem.resolveSupport = {
	properties = {
		'documentation',
		'detail',
		'additionalTextEdits',
	}
}
-- Aplicamos la función anterior a todos los tipos de archivos
require('lspconfig').util.default_config = vim.tbl_extend(
	"force",
	require('lspconfig').util.default_config,
	{
		capabilities = capabilities,
	}
)

-- Declaración de los lenguajes y sus respectivos servidores a usar
-- Tratar de que estén en orden alfabético
-- Agregamos on_attach=require'completion'.on_attach para el plugin completion-nvim
require('lspconfig').bashls.setup{on_attach=require'completion'.on_attach}	-- Bash
-- require('lspconfig').beancount.setup{on_attach=require'completion'.on_attach}	-- Beancount
-- require('lspconfig').ccls.setup{}	-- C language
require('lspconfig').clangd.setup{on_attach=require'completion'.on_attach}	-- C language
require('lspconfig').cmake.setup{on_attach=require'completion'.on_attach} -- C make
require('lspconfig').cssls.setup{on_attach=require'completion'.on_attach} -- CSS
-- require('lspconfig').codeqlls.setup{} -- CodeQL
-- require('lspconfig').dartls.setup{on_attach=require'completion'.on_attach} -- Dart
require('lspconfig').dockerls.setup{on_attach=require'completion'.on_attach} -- Docker
require('lspconfig').fortls.setup{on_attach=require'completion'.on_attach} -- Fortran
require('lspconfig').hls.setup{on_attach=require'completion'.on_attach} -- Haskell
require('lspconfig').html.setup{on_attach=require'completion'.on_attach} -- HTML
require('lspconfig').jsonls.setup{on_attach=require'completion'.on_attach} -- JSON
require('lspconfig').texlab.setup{on_attach=require'completion'.on_attach} -- LaTeX
require('lspconfig').intelephense.setup{on_attach=require'completion'.on_attach} -- PHP
require('lspconfig').jedi_language_server.setup{on_attach=require'completion'.on_attach} -- Python
-- require('lspconfig').pyls.setup{} -- Python
-- require'lspconfig'.solargraph.setup{on_attach=require'completion'.on_attach} -- Ruby
-- require('lspconfig').rls.setup{} -- Rust
require('lspconfig').rls.setup{on_attach=require'completion'.on_attach} -- Rust
require('lspconfig').stylelint_lsp.setup{on_attach=require'completion'.on_attach} -- Stylelint, mejor estilo de código
require('lspconfig').vimls.setup{on_attach=require'completion'.on_attach} -- Vim Language
require('lspconfig').yamlls.setup{on_attach=require'completion'.on_attach} -- YAML

-- SQL
require('lspconfig').sqlls.setup{
	cmd={ "/usr/bin/sql-language-server", "up", "--method", "stdio" },
	on_attach=require'completion'.on_attach
}

-- Java, Necesario paru -S jdtls
-- require('lspconfig').jdtls.setup{on_attach=require'completion'.on_attach}

-- Lua. Necesario paru -S lua-language-server
EOF
" luafile $HOME/.config/nvim/plug-config/lua-lsp-config.lua
lua << EOF
-- require('lspconfig').sumneko_lua.setup{on_attach=require'completion'.on_attach}

-- GO. Necesario sudo pacman -S gopls
require('lspconfig').gopls.setup{on_attach=require'completion'.on_attach}

-- zeta-note. Markdown syntax
require('lspconfig').zeta_note.setup{
	cmd = {'/home/luisbarrera/zeta-note-linux'},
	on_attach=require'completion'.on_attach
}
EOF

" Declaración de keymappings
nnoremap <silent> gd <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent> gD <cmd>lua vim.lsp.buf.declaration()<CR>
nnoremap <silent> gr <cmd>lua vim.lsp.buf.references()<CR>
nnoremap <silent> gi <cmd>lua vim.lsp.buf.implementation()<CR>
nnoremap <silent> K <cmd>lua vim.lsp.buf.hover()<CR>
" nnoremap <silent> <C-k> <cmd>lua vim.lsp.buf.signature_help()<CR>
nnoremap <silent> <C-n> <cmd>lua vim.lsp.diagnostic.goto_prev()<CR>
nnoremap <silent> <C-p> <cmd>lua vim.lsp.diagnostic.goto_next()<CR>

" auto-format
" autocmd BufWritePre *.js lua vim.lsp.buf.formatting_sync(nil, 100)
" autocmd BufWritePre *.jsx lua vim.lsp.buf.formatting_sync(nil, 100)
" autocmd BufWritePre *.py lua vim.lsp.buf.formatting_sync(nil, 100)

" Mappings para compe
" inoremap <silent><expr> <C-Space> compe#complete()
" inoremap <silent><expr> <CR> compe#confirm('<CR>')
" inoremap <silent><expr> <C-e> compe#close('<C-e>')
" inoremap <silent><expr> <C-f> compe#scroll({ 'delta': +4 })
" inoremap <silent><expr> <C-d> compe#scroll({ 'delta': -4 })

highlight link CompeDocumentation NormalFloat
