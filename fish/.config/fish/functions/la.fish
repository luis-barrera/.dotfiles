function la --wraps='exa -la --icons --group-directories-first -h --git' --description 'alias la=exa -la --icons --group-directories-first -h --git'
  exa -la --icons --group-directories-first -h --git $argv; 
end
