local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
  vim.cmd [[packadd packer.nvim]]
end

local status, packer = pcall(require, "packer")

if (not status) then
  print("Packer is not installed")
  return
end

vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
  use 'wbthomason/packer.nvim'

  -- LSP
  use 'neovim/nvim-lspconfig' -- Configurations for Nvim LSP

  -- LSP UI
  use({
    "glepnir/lspsaga.nvim",
    branch = "main"
  })

  -- LSP para no lenuajes sin soporte
  use 'jose-elias-alvarez/null-ls.nvim'
  use 'MunifTanjim/prettier.nvim'

  -- Autocompletado
  use 'onsails/lspkind-nvim'
  use 'L3MON4D3/LuaSnip'
  use 'hrsh7th/cmp-nvim-lsp'
  use 'hrsh7th/cmp-buffer'
  use 'hrsh7th/nvim-cmp'
  use {
    'ray-x/cmp-treesitter',
    requires = { {'hrsh7th/nvim-cmp'} }
  }

  -- Tree-sitter
  use {
    'nvim-treesitter/nvim-treesitter',
    run = function() require('nvim-treesitter.install').update({ with_sync = true }) end,
  }

  -- Autopairs y autotags
  use 'windwp/nvim-ts-autotag'
  use 'windwp/nvim-autopairs'

  -- Fuzzyfinder de archivos por su contenido
  use {
    'nvim-telescope/telescope.nvim', tag = '0.1.0',
    -- or                            , branch = '0.1.x',
    requires = { {'nvim-lua/plenary.nvim'} }
  }
  use { "nvim-telescope/telescope-file-browser.nvim" }

  -- Fuzzy para otros plugins
  use {'nvim-telescope/telescope-fzf-native.nvim', run = 'make'}
  use {'tzachar/fuzzy.nvim', requires = {'nvim-telescope/telescope-fzf-native.nvim'}}

  -- Explorador de archivos
  use { 'kyazdani42/nvim-tree.lua',
        requires = { 'kyazdani42/nvim-web-devicons' }}

  -- Soporte para tailwindcss
  use { "williamboman/mason.nvim",
        "williamboman/mason-lspconfig.nvim" }

  -- Git
  use { 'lewis6991/gitsigns.nvim',
        'dinhhuy258/git.nvim' }

  -- Commenter
  use 'numToStr/Comment.nvim'
  use 'JoosepAlviste/nvim-ts-context-commentstring'

  -- Surround
  use({ "kylechui/nvim-surround", tag = "*" })

  -- Lista de ToDos
  use {
    "folke/todo-comments.nvim",
    requires = "nvim-lua/plenary.nvim"
  }

  -- Statusline
  use {
    'nvim-lualine/lualine.nvim',
    requires = { 'kyazdani42/nvim-web-devicons', opt = true }
  }

  -- DevIcons
  use 'kyazdani42/nvim-web-devicons'

  -- Colorscheme
  use "ellisonleao/gruvbox.nvim"
  use {'luisiacc/gruvbox-baby', branch = 'main'}

  -- Bugger Tabs tags
  use {
    'akinsho/bufferline.nvim',
    tag = "v2.*",
    requires = 'kyazdani42/nvim-web-devicons'
  }

  -- Svelte
  use 'pangloss/vim-javascript'
  use 'maxmellon/vim-jsx-pretty'
  use 'w0rp/ale'
  use 'burner/vim-svelte'

  if packer_bootstrap then
    require('packer').sync()
  end
end)
