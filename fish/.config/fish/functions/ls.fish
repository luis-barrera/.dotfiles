function ls --wraps='exa -l --icons --group-directories-first -h' --description 'alias ls=exa -l --icons --group-directories-first -h'
  exa -l --icons --group-directories-first -h $argv; 
end
