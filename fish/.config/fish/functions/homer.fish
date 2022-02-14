function homer --wraps='ssh 192.168.0.8' --description 'alias homer=ssh 192.168.0.8'
  ssh 192.168.0.8 $argv; 
end
