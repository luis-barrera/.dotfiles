function wifi-up --wraps='sudo ip link set wlan0 up' --description 'alias wifi-up=sudo ip link set wlan0 up'
  sudo ip link set wlan0 up $argv; 
end
