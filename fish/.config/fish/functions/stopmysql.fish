function stopmysql --wraps='systemctl disable mysqld.service --now' --description 'alias stopmysql=systemctl disable mysqld.service --now'
  systemctl disable mysqld.service --now $argv; 
end
