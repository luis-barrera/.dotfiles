function set-firefox-default-browser --wraps='xdg-settings set default-web-browser firefox.desktop' --description 'alias set-firefox-default-browser=xdg-settings set default-web-browser firefox.desktop'
  xdg-settings set default-web-browser firefox.desktop $argv; 
end
