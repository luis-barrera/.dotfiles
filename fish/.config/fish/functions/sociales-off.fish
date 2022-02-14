function sociales-off --wraps=sudo\ sed\ -i\ \'/.\*facebook.com.\*/c\\0.0.0.0\ www.facebook.com\'\ /etc/hosts --description alias\ sociales-off=sudo\ sed\ -i\ \'/.\*facebook.com.\*/c\\0.0.0.0\ www.facebook.com\'\ /etc/hosts
  sudo sed -i '/.*facebook.com.*/c\0.0.0.0 www.facebook.com' /etc/hosts $argv; 
end
