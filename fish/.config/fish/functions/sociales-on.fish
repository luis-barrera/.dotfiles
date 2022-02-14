function sociales-on --wraps=sudo\ sed\ -i\ \'/.\*facebook.com.\*/c\\\#0.0.0.0\ www.facebook.com\'\ /etc/hosts --description alias\ sociales-on=sudo\ sed\ -i\ \'/.\*facebook.com.\*/c\\\#0.0.0.0\ www.facebook.com\'\ /etc/hosts
  sudo sed -i '/.*facebook.com.*/c\#0.0.0.0 www.facebook.com' /etc/hosts $argv; 
end
