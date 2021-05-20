-- Configuración de NeoVim que permite mostrar la ventana de autocompletado
vim.o.completeopt = "menuone,noselect"

require'compe'.setup{
  enabled = true;
  autocomplete = true;  -- Abre la popup automáticamente
  debug = false;
  min_length = 1; -- Mínimo de caracteres insertados necesarios
  preselect = 'enable';
  throttle_time = 80;
  source_timeout = 200;
  incomplete_delay = 400;
  max_abbr_width = 80;
  max_kind_width = 20;
  max_menu_width = 100;
  documentation = true;
  source = {
    path = true;  -- Autocompletado de rutas de archivos
    buffer = true;  -- Autocompletado según el contenido del buffer
    tags = true;  -- Autocompletado con las tags del proyecto
    calc = true;  -- Hacer cálculos y sugerir el resultado
    vsnip = true; -- Autocompletado para snippets
    nvim_lsp = true;  -- Autocompletado según el LSP de NVim
    nvim_lua = true;  -- Autocompletado para Lua
    vsnip = true; -- Snippets
    utilsnip = true;  -- Más snippets
    snippets_nvim = true;
    spell = true;
    treesitter = true;
  };
}
