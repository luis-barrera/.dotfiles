let g:rbpt_colorpairs = [
    \ ['brown',       "#8DA1B9"],
    \ ['Darkblue',    "#06b6ef"],
    \ ['darkgray',    "#815ba4"],
    \ ['darkgreen',   "#D1D646"],
    \ ['darkcyan',    "#DBC7BE"],
    \ ['darkred',     "#8f9d6a"],
    \ ['darkmagenta', "#5990ec"],
    \ ['brown',       "#95ADB6"],
    \ ['gray',        '#cc99cc'],
    \ ['black',       '#e4dab7'],
    \ ['darkmagenta', '#8B939C'],
    \ ['Darkblue',    '#8AEA92'],
    \ ['darkgreen',   '#a49339'],
    \ ['darkcyan',    '#449156'],
    \ ['darkred',     '#e16b4b'],
    \ ['red',         '#F0C808'],
    \ ]

let g:rbpt_max = 16

let g:rbpt_loadcmd_toggle = 0

" Always On
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces
