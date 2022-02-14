function killmysqld --wraps='killall mysqld_safe' --description 'alias killmysqld=killall mysqld_safe'
  killall mysqld_safe $argv; 
end
