function internet-inspect --wraps='ss -p' --description 'alias internet-inspect=ss -p'
  ss -p $argv; 
end
