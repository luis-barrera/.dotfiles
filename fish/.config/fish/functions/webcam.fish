function webcam --wraps='mpv av://v4l2:/dev/video0 --profile=low-latency --untimed' --description 'alias webcam=mpv av://v4l2:/dev/video0 --profile=low-latency --untimed'
  mpv av://v4l2:/dev/video0 --profile=low-latency --untimed $argv; 
end
