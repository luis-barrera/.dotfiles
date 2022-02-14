function nvimconfig --wraps='cd ~/.config/nvim && nvim ~/.config/nvim/init.vim' --description 'alias nvimconfig=cd ~/.config/nvim && nvim ~/.config/nvim/init.vim'
  cd ~/.config/nvim && nvim ~/.config/nvim/init.vim $argv; 
end
