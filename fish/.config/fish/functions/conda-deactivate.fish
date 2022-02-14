function conda-deactivate --wraps='source /opt/anaconda/bin/deactivate root' --description 'alias conda-deactivate=source /opt/anaconda/bin/deactivate root'
  source /opt/anaconda/bin/deactivate root $argv; 
end
