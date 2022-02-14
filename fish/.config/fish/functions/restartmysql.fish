function restartmysql --wraps='systemctl restart mysqld.service' --description 'alias restartmysql=systemctl restart mysqld.service'
  systemctl restart mysqld.service $argv; 
end
