function mysqld --wraps='sudo /usr/local/mysql/bin/mysqld_safe --user=mysql' --description 'alias mysqld=sudo /usr/local/mysql/bin/mysqld_safe --user=mysql'
  sudo /usr/local/mysql/bin/mysqld_safe --user=mysql $argv; 
end
