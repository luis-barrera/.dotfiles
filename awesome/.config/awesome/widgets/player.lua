--[[
  Player Widget
]]--

local wibox    = require("wibox")
local awful    = require("awful")
local markup   = require("markup")
local gears    = require("gears")
local dpi      = require("beautiful.xresources").apply_dpi

local function factory(args)
  local applets_font                 = "Iosevka Custom 10"
  local applets_fg                  = "#000000"
  local applets_bg_1                = "#dd8a6f"
  local applets_bg_2                = "#bedc87"
  local applets_bg_3                = "#b8b5db"
  local bg_systray                  = applets_bg_1 
  local systray_icon_spacing        = dpi(2)
  local applets_spacing             = dpi(2)

  -- Información multimedia
  local songInfo = wibox.widget.textbox(markup.fontfg(applets_font, applets_fg,  " Nada en reprodución "))
  songInfo = awful.widget.watch('sh ~/scripts/songInfo.sh', 1,
    function(songInfo, stdout, stderr, exitreason, exitcode)
      local f = io.popen('sh ~/scripts/songInfo.sh', 'r') 
      local l = f:read()
      songInfo:set_markup_silently(markup.fontfg(applets_font, applets_fg,  l))
      f:close()
    end, wibox.widget.textbox()
  )
  songInfo = wibox.container.margin(songInfo, dpi(0), dpi(0), dpi(2), dpi(2))
   
  -- Botones de control multimedia
  local prev_icon = wibox.widget.textbox(markup.fontfg(applets_font, applets_fg,  ""))
  local next_icon = wibox.widget.textbox(markup.fontfg(applets_font, applets_fg,  ""))
  local song_status_icon = wibox.widget.textbox(markup.fontfg(applets_font, applets_fg,  "  "))
   
  song_status_icon = awful.widget.watch("playerctl status", 1,
    function(song_status_icon, stdout, stderr, exitreason, exitcode)
      local f = io.popen("playerctl status", 'r') 
      local l = f:read()
      if (l == "Playing") then
        song_status_icon:set_markup_silently(markup.fontfg(applets_font, applets_fg,  "  "))
      elseif (l == "Paused") then
        song_status_icon:set_markup(markup.fontfg(applets_font, applets_fg,  "  "))
      else
        song_status_icon:set_markup(markup.fontfg(applets_font, applets_fg,  "  "))
      end
      f:close()
    end, wibox.widget.textbox()
  )
   
  local player = wibox.widget{
    songInfo,
    prev_icon,
    song_status_icon,
    next_icon,
    layout = wibox.layout.fixed.horizontal
  }
   
  -- Funcionalidad de los botones de audio
  prev_icon:connect_signal("button::press", function() os.execute("playerctl --player=playerctld previous") end)
  song_status_icon:connect_signal("button::press", 
    function()
    os.execute("playerctl --player=playerctld play-pause")
    end)
  next_icon:connect_signal("button::press", function() os.execute("playerctl --player=playerctld next") end)
  return player

end

return factory
