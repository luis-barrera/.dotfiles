function firefoxprofile --wraps='firefox -no-remote -P privacy-profile' --description 'alias firefoxprofile=firefox -no-remote -P privacy-profile'
  firefox -no-remote -P privacy-profile $argv; 
end
