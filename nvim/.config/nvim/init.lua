-- TODO: Configurar los status de llamar a cirtos plugins para mostrar un mensaje
-- si no se cargó alguno
--    if (not status) then return end

-- Config antigua de neovim

--[[
  vim.g.my_option = "my value" for setting global vim options.
  vim.o.my_option = "my value" for setting other options.
  vim.bo.my_option = "my value" for setting buffer-specific options.
  vim.wo.my_option = "my value" for setting window-specific options. ]]

-- Desactivar por completo netrw para usar mejor nvim-tree
vim.g.loaded = 1
vim.g.loaded_netrwPlugin = 1

vim.cmd [[
"    ____      _ __        _
"   /  _/___  (_) /__   __(_)___ ___
"   / // __ \/ / __/ | / / / __ `__ \
" _/ // / / / / /__| |/ / / / / / / /
"/___/_/ /_/_/\__(_)___/_/_/ /_/ /_/
" ===================================

" =============================
" = Configuraciones iniciales =
" =============================
" LeaderKey, usar Space para hacer combinaciones de teclas
let mapleader=' '

" Decirle a NeoVim que deje de intentar ser Vi
set nocompatible
" Portapapeles
set clipboard=unnamedplus

" Trata de adivinar el tipo de archivo
filetype plugin on
" Resaltado de sintaxis para cada lenguaje
syntax enable

" Codificación de los caracteres que podemos usar dentro del editor
set encoding=utf-8
" Considera las palabras unidas con - y _ como una palabra
set iskeyword+=-,_

" Siempre muestra la linea de estado
set laststatus=2
" Muestra siempre el modo en el que estamos
set showmode
" Muestra los comandos que damos
set showcmd
" Tamano de la ventana de comandos
set cmdheight=1
" Pide confirmacion para guardar el buffer antes de cerrar el editor
set confirm

" Tab-Completion cuando buscamos un archivo en el modo comando
" set path+=/home/luisbarrera/,**
set path+=**
" Mejor autocompletado en la línea de comandos
set wildmenu wildmode=longest:full,full

" Búsqueda dentro del archivo
" Búsqueda insensible a las mayúsculas
set ignorecase
" Búsqueda sensible a las mayúsculas solo si se usan mayúsculas
set smartcase
" Busca al mismo tiempo que se ingresa la palabra
set incsearch
" Resalta los resultados de búsqueda
set hlsearch

" Indentado
" Sigue la indentación de la linea anterior
set autoindent
" Retroceder en autoindentado, fin de línea o nueva línea
set backspace=indent,eol,start
" Que el cursor se ponga sobre el primer char de la línea
set nostartofline

" Muestra en colores los brackets
set showmatch
" Advierte visualmente si un problema ocurre
set visualbell
" Ayuda a que la advertencia visual no de falsos
set t_vb=

" Permite usar el mouse en todos los modos
set mouse=a

" Columna de números y signos
" Número de línea a la izquierda
set number
" Número de línea respecto a la línea donde esta el cursor
set relativenumber
" Tamaño de la columna de número
set numberwidth=2
" Muestra una columna con signos, se usa en git y otras cosas
set signcolumn=auto

" Muestra las línea largas como varias líneas
set nowrap
" Para que el cursor se salte a la línea si estamos en el último char
set whichwrap+=<,>,[,],h,l
" Rompe la línea respecto a un espacio y no entre palabras
set linebreak
" Al hacer wrap de una línea, sigue manteniendo el mismo indent
set breakindent

" Mostrar varios editores en una misma ventana, ocultando uno mientras se
"   muestra otro
" set hidden
" Muestra 5 lineas mas cuando se cuando se navega verticalmente archivo
set scrolloff=5
" Muestra el nombre del archivo en el titulo de la ventana
set title
" Máximo de pestanas
" set tabpagemax=10
" Muestra la barra de pestañas abiertas siempre
set showtabline=2

" Dividir ventanas
" Si dividimos la ventana horizontalmente, aparece abajo
set splitbelow
" Si dividimos la ventana verticalmente, aparece a la derecha
set splitright

" Tasa de refresco del buffer
set updatetime=300
" Retardo en las teclas para acceder una combinación de teclas extra
set timeout
" No retardo en los mapeos de teclas
set nottimeout
" Milisegundos de retardo en las teclas
set ttimeoutlen=500

" Agrupar código, con zc y zo
set foldmethod=indent

" Para que los temas no tengan problemas con la signcolumn de gitgutter
highlight! link SignColumn LineNr

" Columna de color mamalona
set colorcolumn=80
highlight ColorColumn guifg=#000000 guibg=#BD1E1E
]]


require('plugins')

-- Colorscheme
-- require("gruvbox").setup({
--   undercurl = true,
--   underline = true,
--   bold = true,
--   italic = true,
--   strikethrough = true,
--   invert_selection = false,
--   invert_signs = false,
--   invert_tabline = false,
--   invert_intend_guides = false,
--   inverse = true, -- invert background for search, diffs, statuslines and errors
--   contrast = "hard", -- can be "hard", "soft" or empty string
--   overrides = {},
-- })
-- vim.cmd("colorscheme gruvbox")

-- Enable telescope theme
vim.g.gruvbox_baby_telescope_theme = 1
-- Transparent mode
vim.g.gruvbox_baby_transparent_mode = 0
-- Background color
vim.g.gruvbox_baby_background_color = 'dark'
vim.cmd[[colorscheme gruvbox-baby]]

-- Statusline
require('lualine').setup {
  options = {
    icons_enabled = true,
    theme = 'gruvbox-baby',
    component_separators = { left = '', right = ''},
    section_separators = { left = '', right = ''},
    disabled_filetypes = {
      statusline = {},
      winbar = {},
    },
    ignore_focus = {},
    always_divide_middle = true,
    globalstatus = false,
    refresh = {
      statusline = 1000,
      tabline = 1000,
      winbar = 1000,
    }
  },
  sections = {
    lualine_a = {'mode'},
    lualine_b = {'branch', 'diff', 'diagnostics'},
    lualine_c = {'filename'},
    lualine_x = {'encoding', 'fileformat', 'filetype'},
    lualine_y = {'progress'},
    lualine_z = {'location'}
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {'filename'},
    lualine_x = {'location'},
    lualine_y = {},
    lualine_z = {}
  },
  tabline = {},
  winbar = {},
  inactive_winbar = {},
  extensions = {}
}

-- LSP
local status, nvim_lsp = pcall(require, "lspconfig")
-- if (not status) then return end

-- LSP TypeScript
local protocol = require('vim.lsp.protocol')

local on_attach = function(client, bufnr)
  -- format on save
  if client.server_capabilities.documentFormattingProvider then
    vim.api.nvim_create_autocmd("BufWritePre", {
      group = vim.api.nvim_create_augroup("Format", { clear = true }),
      buffer = bufnr,
      callback = function() vim.lsp.buf.formatting_seq_sync() end
    })
  end
end
nvim_lsp.tsserver.setup {
  -- on_attach = on_attach,
  filetypes = { "typescript", "typescriptreact", "typescript.tsx" },
  cmd = { "typescript-language-server", "--stdio" }
} 

nvim_lsp.eslint.setup{
  filetypes = { "javascript", "javascriptreact", "javascript.jsx", "typescript", "typescriptreact", "typescript.tsx", "vue" },
  cmd = { "vscode-eslint-language-server", "--stdio" }
}

-- Autocompletado
local status, cmp = pcall(require, "cmp")
if (not status) then return end
local lspkind = require 'lspkind'

cmp.setup({
  snippet = {
    expand = function(args)
      require('luasnip').lsp_expand(args.body)
    end,
  },
  mapping = cmp.mapping.preset.insert({
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.close(),
    ['<CR>'] = cmp.mapping.confirm({
      behavior = cmp.ConfirmBehavior.Replace,
      select = true
    }),
  }),
  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    { name = 'buffer' },
    { name = 'treesitter'},
  }),
  formatting = {
    format = lspkind.cmp_format({ with_text = false, maxwidth = 50 })
  }
})

vim.cmd [[
  set completeopt=menuone,noinsert,noselect
  highlight! default link CmpItemKind CmpItemMenuDefault
]]

vim.cmd ([[
	augroup packer_user_config
		autocmd!
	autocmd BufWritePost plugins.lua source <afile> | PackerCompile
		augroup end
]])

-- Autocompletado
local status, cmp = pcall(require, "cmp")
if (not status) then return end
local lspkind = require 'lspkind'

cmp.setup({
  snippet = {
    expand = function(args)
      require('luasnip').lsp_expand(args.body)
    end,
  },
  mapping = cmp.mapping.preset.insert({
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.close(),
    ['<CR>'] = cmp.mapping.confirm({
      behavior = cmp.ConfirmBehavior.Replace,
      select = true
    }),
  }),
  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    { name = 'buffer' },
  }),
  formatting = {
    format = lspkind.cmp_format({ with_text = false, maxwidth = 50 })
  }
})

vim.cmd [[
  set completeopt=menuone,noinsert,noselect
  highlight! default link CmpItemKind CmpItemMenuDefault
]]

-- Treesitter
local status, ts = pcall(require, "nvim-treesitter.configs")
if (not status) then return end

ts.setup {
  highlight = {
    enable = true,
    disable = {},
  },
  indent = {
    enable = true,
    disable = {},
  },
  ensure_installed = {
    "tsx",
    "javascript",
    "toml",
    "fish",
    "php",
    "json",
    "yaml",
    "swift",
    "css",
    "html",
    "lua"
  },
  autotag = {
    enable = true,
  },
}

local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
parser_config.tsx.filetype_to_parsername = { "javascript", "typescript.tsx" }

-- vim.opt.foldmethod     = 'expr'
-- vim.opt.foldexpr       = 'nvim_treesitter#foldexpr()'
---WORKAROUND
vim.api.nvim_create_autocmd({'BufEnter','BufAdd','BufNew','BufNewFile','BufWinEnter'}, {
  group = vim.api.nvim_create_augroup('TS_FOLD_WORKAROUND', {}),
  callback = function()
    vim.opt.foldmethod     = 'expr'
    vim.opt.foldexpr       = 'nvim_treesitter#foldexpr()'
  end
})
---ENDWORKAROUND 

-- Auto close tags
local status, autotag = pcall(require, "nvim-ts-autotag")
if (not status) then return end

autotag.setup({}) 

-- Auto close pairs
local status, autopairs = pcall(require, "nvim-autopairs")
if (not status) then return end

autopairs.setup({
  disable_filetype = { "TelescopePrompt" , "vim" },
}) 

-- Telescope
local status, telescope = pcall(require, "telescope")
if (not status) then return end
local actions = require('telescope.actions')
local builtin = require("telescope.builtin")

local function telescope_buffer_dir()
  return vim.fn.expand('%:p:h')
end

local fb_actions = require "telescope".extensions.file_browser.actions

telescope.setup {
  defaults = {
    mappings = {
      n = {
        ["q"] = actions.close
      },
    },
    -- Para el tema de gruvbox-baby
    borderchars = {
      prompt = { "─", " ", " ", " ", "─", "─", " ", " " },
      results = { " " },
      preview = { " " },
    },
  },
}

-- keymaps
vim.keymap.set('n', ';f',
  function()
    builtin.find_files({
      no_ignore = false,
      hidden = true
    })
  end)
vim.keymap.set('n', ';r', function()
  builtin.live_grep()
end)
vim.keymap.set('n', '\\\\', function()
  builtin.buffers()
end)
vim.keymap.set('n', ';t', function()
  builtin.help_tags()
end)
vim.keymap.set('n', ';;', function()
  builtin.resume()
end)
vim.keymap.set('n', ';e', function()
  builtin.diagnostics()
end)

telescope.setup {
  defaults = {
    mappings = {
      n = {
        ["q"] = actions.close
      },
    },
  },
  extensions = {
    file_browser = {
      theme = "dropdown",
      -- disables netrw and use telescope-file-browser in its place
      hijack_netrw = true,
      mappings = {
        -- your custom insert mode mappings
        ["i"] = {
          ["<C-w>"] = function() vim.cmd('normal vbd') end,
        },
        ["n"] = {
          -- your custom normal mode mappings
          ["N"] = fb_actions.create,
          ["h"] = fb_actions.goto_parent_dir,
          ["/"] = function()
            vim.cmd('startinsert')
          end
        },
      },
    },
  },
}
telescope.load_extension("file_browser")

vim.keymap.set("n", "sf", function()
  telescope.extensions.file_browser.file_browser({
    path = "%:p:h",
    cwd = telescope_buffer_dir(),
    respect_gitignore = false,
    hidden = true,
    grouped = true,
    previewer = false,
    initial_mode = "normal",
    layout_config = { height = 40 }
  })
end)

-- Tabs
vim.opt.termguicolors = true
require("bufferline").setup{
  options = {
    mode = "tabs",
    numbers = "ordinal",
    separator_style = 'slant',
    diagnostics = "nvim_lsp",
    always_show_bufferline = false,
    show_buffer_close_icons = false,
    show_close_icon = false,
    color_icons = true,
    offsets = {
      {
        filetype = "NvimTree",
        text = "File Explorer",
        highlight = "Directory",
        separator = true -- use a "true" to enable the default, or set your own character
      },
    },
  },
}

-- LSP UI
local status, saga = pcall(require, "lspsaga")
if (not status) then return end

saga.init_lsp_saga {
  server_filetype_map = {
    typescript = 'typescript'
  }
}

local opts = { noremap = true, silent = true }
vim.keymap.set('n', '<C-j>', '<Cmd>Lspsaga diagnostic_jump_next<CR>', opts)
vim.keymap.set('n', 'K', '<Cmd>Lspsaga hover_doc<CR>', opts)
vim.keymap.set('n', 'gd', '<Cmd>Lspsaga lsp_finder<CR>', opts)
vim.keymap.set('i', '<C-k>', '<Cmd>Lspsaga signature_help<CR>', opts)
vim.keymap.set('n', 'gp', '<Cmd>Lspsaga preview_definition<CR>', opts)
vim.keymap.set('n', 'gr', '<Cmd>Lspsaga rename<CR>', opts) 

-- LSP no nativo y prettier
local status, null_ls = pcall(require, "null-ls")
if (not status) then return end

null_ls.setup({
  sources = {
    null_ls.builtins.diagnostics.eslint_d.with({
      diagnostics_format = '[eslint] #{m}\n(#{c})'
    }),
    null_ls.builtins.diagnostics.fish
  }
})

local status, prettier = pcall(require, "prettier")
if (not status) then return end

prettier.setup {
  bin = 'prettierd',
  filetypes = {
    "css",
    "javascript",
    "javascriptreact",
    "typescript",
    "typescriptreact",
    "json",
    "scss",
    "less"
  }
}

-- Git
require('gitsigns').setup {}

local status, git = pcall(require, "git")
if (not status) then return end

git.setup({
  keymaps = {
    -- Open blame window
    blame = "<Leader>gb",
    -- Open file/folder in git repository
    browse = "<Leader>go",
  }
})

-- Soporte para Tailwindcss
local status, mason = pcall(require, "mason")
if (not status) then return end
local status2, lspconfig = pcall(require, "mason-lspconfig")
if (not status2) then return end

mason.setup({})

lspconfig.setup {
  ensure_installed = { "sumneko_lua", "tailwindcss" },
}

nvim_lsp.tailwindcss.setup {}

-- Commenter
require('Comment').setup {
    ---Add a space b/w comment and the line
    padding = true,
    ---Whether the cursor should stay at its position
    sticky = true,
    ---Lines to be ignored while (un)comment
    ignore = nil,
    ---LHS of toggle mappings in NORMAL mode
    toggler = {
        ---Line-comment toggle keymap
        line = 'gcc',
        ---Block-comment toggle keymap
        block = 'gbc',
    },
    ---LHS of operator-pending mappings in NORMAL and VISUAL mode
    opleader = {
        ---Line-comment keymap
        line = 'gc',
        ---Block-comment keymap
        block = 'gb',
    },
    ---LHS of extra mappings
    extra = {
        ---Add comment on the line above
        above = 'gcO',
        ---Add comment on the line below
        below = 'gco',
        ---Add comment at the end of line
        eol = 'gcA',
    },
    ---Enable keybindings
    ---NOTE: If given `false` then the plugin won't create any mappings
    mappings = {
        ---Operator-pending mapping; `gcc` `gbc` `gc[count]{motion}` `gb[count]{motion}`
        basic = true,
        ---Extra mapping; `gco`, `gcO`, `gcA`
        extra = true,
        ---Extended mapping; `g>` `g<` `g>[count]{motion}` `g<[count]{motion}`
        extended = false,
    },
    ---Function to call before (un)comment
    pre_hook = require('ts_context_commentstring.integrations.comment_nvim').create_pre_hook(),
    ---Function to call after (un)comment
    post_hook = nil,
}

require'nvim-treesitter.configs'.setup {
  context_commentstring = {
    enable = true,
    enable_autocmd = false,
  }
}

-- Surround, manejo de paréntesis
local status, nvim_surround = pcall(require, "nvim-surround")
if (not status) then return end
nvim_surround.setup()

-- Lista de ToDos
local status, todo_comments = pcall(require, "todo-comments")
if (not status) then
  print("Error starting todo-comments")
  return
else
  todo_comments.setup {
    signs = true, -- show icons in the signs column
    sign_priority = 8, -- sign priority
    -- keywords recognized as todo comments
    keywords = {
      FIX = {
        icon = " ", -- icon used for the sign, and in search results
        color = "error", -- can be a hex color, or a named color (see below)
        alt = { "FIXME", "BUG", "FIXIT", "ISSUE" }, -- a set of other keywords that all map to this FIX keywords
        -- signs = false, -- configure signs for some keywords individually
      },
      TODO = { icon = " ", color = "info" },
      HACK = { icon = " ", color = "warning" },
      WARN = { icon = " ", color = "warning", alt = { "WARNING", "XXX" } },
      PERF = { icon = " ", alt = { "OPTIM", "PERFORMANCE", "OPTIMIZE" } },
      NOTE = { icon = " ", color = "hint", alt = { "INFO" } },
      TEST = { icon = "⏲ ", color = "test", alt = { "TESTING", "PASSED", "FAILED" } },
    },
    gui_style = {
      fg = "NONE", -- The gui style to use for the fg highlight group.
      bg = "BOLD", -- The gui style to use for the bg highlight group.
    },
    merge_keywords = true, -- when true, custom keywords will be merged with the defaults
    -- highlighting of the line containing the todo comment
    -- * before: highlights before the keyword (typically comment characters)
    -- * keyword: highlights of the keyword
    -- * after: highlights after the keyword (todo text)
    highlight = {
      before = "", -- "fg" or "bg" or empty
      keyword = "wide", -- "fg", "bg", "wide", "wide_bg", "wide_fg" or empty. (wide and wide_bg is the same as bg, but will also highlight surrounding characters, wide_fg acts accordingly but with fg)
      after = "fg", -- "fg" or "bg" or empty
      pattern = [[.*<(KEYWORDS)\s*:]], -- pattern or table of patterns, used for highlightng (vim regex)
      comments_only = true, -- uses treesitter to match keywords in comments only
      max_line_len = 400, -- ignore lines longer than this
      exclude = {}, -- list of file types to exclude highlighting
    },
    -- list of named colors where we try to extract the guifg from the
    -- list of highlight groups or use the hex color if hl not found as a fallback
    colors = {
      error = { "DiagnosticError", "ErrorMsg", "#DC2626" },
      warning = { "DiagnosticWarning", "WarningMsg", "#FBBF24" },
      info = { "DiagnosticInfo", "#2563EB" },
      hint = { "DiagnosticHint", "#10B981" },
      default = { "Identifier", "#7C3AED" },
      test = { "Identifier", "#FF00FF" }
    },
    search = {
      command = "rg",
      args = {
        "--color=never",
        "--no-heading",
        "--with-filename",
        "--line-number",
        "--column",
      },
      -- regex that will be used to match keywords.
      -- don't replace the (KEYWORDS) placeholder
      pattern = [[\b(KEYWORDS):]], -- ripgrep regex
      -- pattern = [[\b(KEYWORDS)\b]], -- match without the extra colon. You'll likely get false positives
    },
  }
end


-- Explorador de archivos
require("nvim-tree").setup({
  sort_by = "case_sensitive",
  view = {
    adaptive_size = true,
    mappings = {
      list = {
        { key = "u", action = "dir_up" },
      },
    },
  },
  renderer = {
    group_empty = true,
  },
  filters = {
    dotfiles = true,
  },
})

-- Abrir el file explorer
vim.api.nvim_set_keymap(
  '',
  '<Leader>fp',
  ':NvimTreeToggle<CR>',
  { noremap = true, silent = true })


vim.cmd [[
" ========
" = Tabs =
" ========
" Establecer tabstop, softtabstop y shiftwidth al mismo nivel
source $HOME/.config/nvim/functions/stab.vim
source $HOME/.config/nvim/functions/toggleindent.vim
" Convierte los tabs en espacios
" set noexpandtab
set expandtab
set copyindent
set preserveindent
" Convierte la tecla tab en 8 espacios
set tabstop=2
" Detecta mejor si varios espacios son un TAB al momento de borrar
" set softtabstop=0
" Se usan 2 espacios en lugar de tabuladores para indentar
set shiftwidth=2
" Al insertar un TAB después de texto, inserta los 2 espacios
" set smarttab
set listchars+=tab:│•
" set listchars+=eol:¶
" Muestra los tabuladores con un caracter > o el que pongamos con listchars
set list
" Para convertir tabs en espacios en documentos que tienen tabs, usar :retab


" =============
" = Funciones =
" =============
" Sustituir la linea actual con la salida del comando `date`
source $HOME/.config/nvim/functions/getFecha.vim
" Limpiar todos los registros
source $HOME/.config/nvim/functions/clearRegisters.vim


" ====================
" = Mapeos de teclas =
" ====================
" Algunos plugins tienen también sus propios mappings
source $HOME/.config/nvim/keys/mappings.vim


" ============
" = Reconfig =
" ============
" Al parecer hay plugins que sobreescriben las coniguraciones, por eso las
" cambio de lugar. Antes estaban al principio, ahora al final
" Directorio donde guardar el registro de cambios de un archivo
set undodir=$HOME/.config/nvim/undodir

" Desactiva el swapfile, en este archivo se guardan copias de seguridad
set noswapfile
" Activa un archivo de respaldo
set undofile
" No crea archivos de backup
set nobackup
set nowritebackup

" Desactiva que se inserte una línea de comentario si hacemos ^O o ^o en una
"   linea comentada, es un comportamiento que, en lo personal, me disgusta
set formatoptions-=o
autocmd FileType * set formatoptions-=o

" let s:guifontsize = 10
" let s:guifont = "JetBrainsMono\\ Nerd\\ Font"
]]

-- Neovide
vim.g.neovide_transparency=0.8
vim.g.gui_font_default_size = 10
vim.g.gui_font_size = vim.g.gui_font_default_size
vim.g.gui_font_face = "JetBrainsMono Nerd Font Mono"

RefreshGuiFont = function()
  vim.opt.guifont = string.format("%s:h%s",vim.g.gui_font_face, vim.g.gui_font_size)
end

ResizeGuiFont = function(delta)
  vim.g.gui_font_size = vim.g.gui_font_size + delta
  RefreshGuiFont()
end

ResetGuiFont = function ()
  vim.g.gui_font_size = vim.g.gui_font_default_size
  RefreshGuiFont()
end

-- Call function on startup to set default value
ResetGuiFont()

-- Keymaps

local opts = { noremap = true, silent = true }

vim.keymap.set({'n', 'i'}, "<C-+>", function() ResizeGuiFont(1)  end, opts)
vim.keymap.set({'n', 'i'}, "<C-->", function() ResizeGuiFont(-1) end, opts)
vim.keymap.set({'n', 'i'}, "<C-BS>", function() ResetGuiFont() end, opts)
