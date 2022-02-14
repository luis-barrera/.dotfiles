function startmysql --wraps='systemctl enable mysqld.service --now' --description 'alias startmysql=systemctl enable mysqld.service --now'
  systemctl enable mysqld.service --now $argv; 
end
