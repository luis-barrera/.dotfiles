function pscpu --wraps='ps auxf | sort -nr -k 3' --description 'alias pscpu=ps auxf | sort -nr -k 3'
  ps auxf | sort -nr -k 3 $argv; 
end
