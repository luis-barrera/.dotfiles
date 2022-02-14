function set-chromium-default-browser --wraps='xdg-settings set default-web-browser chromium.desktop' --description 'alias set-chromium-default-browser=xdg-settings set default-web-browser chromium.desktop'
  xdg-settings set default-web-browser chromium.desktop $argv; 
end
