



hi Comment cterm=italic
if (has('nvim'))
  let $NVIM_TUI_ENABLE_TRUE_COLOR = 1
endif

if (has('termguicolors'))
  set termguicolors
endif

let g:material_theme_style = 'darker'
let g:material_terminal_italics = 1

syntax on
colorscheme material


" checks if your terminal has 24-bit color support
" if (has("termguicolors"))
"     set termguicolors
"         hi LineNr ctermbg=NONE guibg=NONE
"         endif
