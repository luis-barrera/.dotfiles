function wifi-down --wraps='sudo ip link set wlan0 down' --description 'alias wifi-down=sudo ip link set wlan0 down'
  sudo ip link set wlan0 down $argv; 
end
