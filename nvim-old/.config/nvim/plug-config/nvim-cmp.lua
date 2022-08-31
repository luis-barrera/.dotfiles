-- Setup nvim-cmp.
local cmp = require'cmp'

cmp.setup({
snippet = {
  -- REQUIRED - you must specify a snippet engine
  expand = function(args)
    -- vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
    -- require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
    -- require('snippy').expand_snippet(args.body) -- For `snippy` users.
    vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
  end,
},
window = {
  completion = cmp.config.window.bordered(),
  documentation = cmp.config.window.bordered(),
},
mapping = cmp.mapping.preset.insert({
  ['<C-b>'] = cmp.mapping.scroll_docs(-4),
  ['<C-f>'] = cmp.mapping.scroll_docs(4),
  ['<C-Space>'] = cmp.mapping.complete(),
  ['<C-e>'] = cmp.mapping.abort(),
  ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
}),
sources = cmp.config.sources({
  { name = 'nvim_lsp' },
  -- { name = 'vsnip' }, -- For vsnip users.
  -- { name = 'luasnip' }, -- For luasnip users.
  { name = 'ultisnips' }, -- For ultisnips users.
  -- { name = 'snippy' }, -- For snippy users.
}, {
  { name = 'buffer' },
})
})

-- Set configuration for specific filetype.
cmp.setup.filetype('gitcommit', {
sources = cmp.config.sources({
  { name = 'cmp_git' }, -- You can specify the `cmp_git` source if you were installed it.
}, {
  { name = 'buffer' },
})
})

-- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline('/', {
mapping = cmp.mapping.preset.cmdline(),
sources = {
  { name = 'buffer' }
}
})

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
-- cmp.setup.cmdline(':', {
--   mapping = cmp.mapping.preset.cmdline(),
--   sources = cmp.config.sources({
--     { name = 'path' }
--   }, {
--     { name = 'cmdline' }
--   })
-- })

-- Setup lspconfig.
local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())
-- Replace <YOUR_LSP_SERVER> with each lsp server you've enabled.
-- require('lspconfig')['<YOUR_LSP_SERVER>'].setup {
--   capabilities = capabilities
-- }

-- Declaración de los lenguajes y sus respectivos servidores a usar
-- Tratar de que estén en orden alfabético
-- Agregamos on_attach=require'completion'.on_attach para el plugin completion-nvim
require('lspconfig').bashls.setup{capabilities = capabilities}	-- Bash
-- require('lspconfig').beancount.setup{on_attach=require'completion'.on_attach}	-- Beancount
-- require('lspconfig').ccls.setup{}	-- C language
require('lspconfig').clangd.setup{capabilities = capabilities}	-- C language
require('lspconfig').cmake.setup{capabilities = capabilities} -- C make
require('lspconfig').cssls.setup{capabilities = capabilities} -- CSS
-- require('lspconfig').codeqlls.setup{} -- CodeQL
-- require('lspconfig').dartls.setup{on_attach=require'completion'.on_attach} -- Dart
require('lspconfig').dockerls.setup{capabilities = capabilities} -- Docker
require('lspconfig').fortls.setup{capabilities = capabilities} -- Fortran
require('lspconfig').hls.setup{capabilities = capabilities} -- Haskell
require('lspconfig').html.setup{capabilities = capabilities} -- HTML
require('lspconfig').jsonls.setup{capabilities = capabilities} -- JSON
require('lspconfig').texlab.setup{capabilities = capabilities} -- LaTeX
require('lspconfig').intelephense.setup{capabilities = capabilities} -- PHP
-- require('lspconfig').jedi_language_server.setup{capabilities = capabilities} -- Python
-- require('lspconfig').pyls.setup{} -- Python
-- require'lspconfig'.solargraph.setup{on_attach=require'completion'.on_attach} -- Ruby
-- require('lspconfig').rls.setup{} -- Rust
require('lspconfig').rls.setup{capabilities = capabilities} -- Rust
require('lspconfig').stylelint_lsp.setup{capabilities = capabilities} -- Stylelint, mejor estilo de código
require('lspconfig').vimls.setup{capabilities = capabilities} -- Vim Language
require('lspconfig').yamlls.setup{capabilities = capabilities} -- YAML

-- SQL
require('lspconfig').sqlls.setup{
	cmd={ "/usr/bin/sql-language-server", "up", "--method", "stdio" },
	-- on_attach=require'completion'.on_attach,
	capabilities = capabilities
}

-- Java, Necesario paru -S jdtls
-- require('lspconfig').jdtls.setup{on_attach=require'completion'.on_attach}

-- Lua. Necesario paru -S lua-language-server
-- EOF
-- " luafile $HOME/.config/nvim/plug-config/lua-lsp-config.lua
-- lua << EOF
-- require('lspconfig').sumneko_lua.setup{on_attach=require'completion'.on_attach}

-- GO. Necesario sudo pacman -S gopls
require('lspconfig').gopls.setup{capabilities = capabilities}

-- zeta-note. Markdown syntax
require('lspconfig').zeta_note.setup{
	cmd = {'/home/luisbarrera/zeta-note-linux'},
	-- on_attach=require'completion'.on_attach,
	capabilities = capabilities
}
