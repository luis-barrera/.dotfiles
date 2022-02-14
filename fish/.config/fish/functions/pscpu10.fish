function pscpu10 --wraps='ps auxf | sort -nr -k 3 | head -10' --description 'alias pscpu10=ps auxf | sort -nr -k 3 | head -10'
  ps auxf | sort -nr -k 3 | head -10 $argv; 
end
