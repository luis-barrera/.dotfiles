function samba --wraps='sudo mount -t cifs //192.168.0.8/valbar /mnt/samba-hp -o username=valbar' --description 'alias samba=sudo mount -t cifs //192.168.0.8/valbar /mnt/samba-hp -o username=valbar'
  sudo mount -t cifs //192.168.0.8/valbar /mnt/samba-hp -o username=valbar $argv; 
end
