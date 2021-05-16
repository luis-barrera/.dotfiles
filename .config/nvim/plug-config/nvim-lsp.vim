" Lista de servers del native nvim language server protocol
"   los nombres de los servers disponibles se encuentran en:
"   https://github.com/neovim/nvim-lspconfig/blob/master/CONFIG.md
" Cada uno de estos servers deben ser instalados por separado. Visitar la
"   página anterior para más información.

" Característica necesaria para mostrar la ventana de sugerencias
set omnifunc=compe#complete()

" Inicia bloque de código de Lua, aquí se declaran los lenguajes y sus
"    respectivos servidores a usar en el LSP nativo
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
require'lspconfig'.util.default_config = vim.tbl_extend(
  "force",
  require'lspconfig'.util.default_config,
  {
    capabilities = capabilities,
  }
)

-- Declaración de los lenguajes y sus respectivos servidores a usar
-- Tratar de que estén en orden alfabético
require'lspconfig'.bashls.setup{}  -- Bash
require'lspconfig'.beancount.setup{}  -- Beancount
-- require'lspconfig'.ccls.setup{}  -- C language
require'lspconfig'.clangd.setup{}  -- C language
require'lspconfig'.cmake.setup{} -- C make
require'lspconfig'.cssls.setup{} -- CSS
-- require'lspconfig'.codeqlls.setup{} -- CodeQL
require'lspconfig'.dartls.setup{} -- Dart
require'lspconfig'.dockerls.setup{} -- Docker
require'lspconfig'.fortls.setup{} -- Fortran
require'lspconfig'.hls.setup{} -- Haskell
require'lspconfig'.html.setup{} -- HTML
require'lspconfig'.jsonls.setup{} -- JSON
require'lspconfig'.texlab.setup{} -- LaTeX
require'lspconfig'.intelephense.setup{} -- PHP
require'lspconfig'.jedi_language_server.setup{} -- Python
-- require'lspconfig'.pyls.setup{} -- Python
-- require'lspconfig'.rls.setup{} -- Rust
require'lspconfig'.rust_analyzer.setup{} -- Rust
require'lspconfig'.stylelint_lsp.setup{} -- Stylelint, mejor estilo de código
require'lspconfig'.vimls.setup{} -- Vim Language
require'lspconfig'.yamlls.setup{} -- YAML

-- SQL
require'lspconfig'.sqlls.setup{
    cmd={ "/usr/bin/sql-language-server", "up", "--method", "stdio" };
}
-- Java
-- Necesario paru -S jdtls
require'lspconfig'.jdtls.setup{}

-- TODO LuaScript
-- Necesario paru -S lua-language-server
-- source $HOME/.config/nvim/plug-config/lua-lsp-config.lua
require'lspconfig'.sumneko_lua.setup{}
-- Flow, static type checker for JS
-- TODO instalar el server y configurarlo
require'lspconfig'.flow.setup{}
-- GO
-- TODO instalar y configurar el server
require'lspconfig'.gopls.setup{}
EOF

" Declaración de keymappings
nnoremap <silent> gd <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent> gD <cmd>lua vim.lsp.buf.declaration()<CR>
nnoremap <silent> gr <cmd>lua vim.lsp.buf.references()<CR>
nnoremap <silent> gi <cmd>lua vim.lsp.buf.implementation()<CR>
nnoremap <silent> K <cmd>lua vim.lsp.buf.hover()<CR>
nnoremap <silent> <C-k> <cmd>lua vim.lsp.buf.signature_help()<CR>
nnoremap <silent> <C-n> <cmd>lua vim.lsp.diagnostic.goto_prev()<CR>
nnoremap <silent> <C-p> <cmd>lua vim.lsp.diagnostic.goto_next()<CR>

" auto-format
autocmd BufWritePre *.js lua vim.lsp.buf.formatting_sync(nil, 100)
autocmd BufWritePre *.jsx lua vim.lsp.buf.formatting_sync(nil, 100)
autocmd BufWritePre *.py lua vim.lsp.buf.formatting_sync(nil, 100)

" Mappings para compe
inoremap <silent><expr> <C-Space> compe#complete()
inoremap <silent><expr> <CR>      compe#confirm('<CR>')
inoremap <silent><expr> <C-e>     compe#close('<C-e>')
inoremap <silent><expr> <C-f>     compe#scroll({ 'delta': +4 })
inoremap <silent><expr> <C-d>     compe#scroll({ 'delta': -4 })

" highlight link CompeDocumentation NormalFloat
