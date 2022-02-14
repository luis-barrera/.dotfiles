function nvimplugins --wraps='cd ~/.config/nvim && nvim ~/.config/nvim/vim-plug/plugins.vim' --description 'alias nvimplugins=cd ~/.config/nvim && nvim ~/.config/nvim/vim-plug/plugins.vim'
  cd ~/.config/nvim && nvim ~/.config/nvim/vim-plug/plugins.vim $argv; 
end
