--[[
     Configuración de AwesomeWM de: github.com/luis-barrera
     Inspirado por:
     Awesome WM configuration template en github.com/lcpz
--]]

----------------------------------------------------------------------------------
------------------------------ Librerias necesarias ------------------------------
----------------------------------------------------------------------------------
local string, os = string, os
local gears = require("gears")
local lain  = require("lain")
local awful = require("awful")
local wibox = require("wibox")
local dpi   = require("beautiful.xresources").apply_dpi
local my_table = awful.util.table or gears.table
local watch = require("awful.widget.watch")
local batteryarc = require("widgets.batteryarc")
local calnot = require("widgets.cal")
local markup = lain.util.markup

----------------------------------------------------------------------------------
---------------------------- Definición de varibles ------------------------------
----------------------------------------------------------------------------------
local theme                       = {}
-- Directorios por default
theme.default_dir                 = require("awful.util").get_themes_dir() .. "default"
theme.wallpaper                   = os.getenv("HOME") .. "/Imágenes/fondo2.png"
theme.wallpaper2                  = os.getenv("HOME") .. "/Imágenes/f5.png"
-- Fuentes
theme.font                        = "Iosevka Custom 9"
theme.player_font                 = "Hurmit Nerd Font Mono 10"
theme.taglist_font                = "Iosevka Custom Bold 10"
-- Colores
theme.fg_normal                   = "#FFFFFF"
theme.fg_focus                    = "#f6832f"
theme.bg_focus                    = "#000000"
theme.bg_normal                   = "#000000"
theme.fg_urgent                   = "#f8ccae"
theme.bg_urgent                   = "#f64a32"
theme.fg_player                   = "#bedc87"
-- Colores de los applets
theme.applets_font                 = "Iosevka Custom 10"
theme.applets_fg                  = "#000000"
theme.applets_bg_1                = "#dd8a6f"
theme.applets_bg_2                = "#bedc87"
theme.applets_bg_3                = "#b8b5db"
theme.bg_systray                  = theme.applets_bg_1 
theme.systray_icon_spacing        = dpi(2)
theme.applets_spacing             = dpi(2)
-- Bordes de clientes
theme.border_width                = dpi(3)
theme.border_normal               = "#000000"
theme.border_focus                = "#f6832f"
-- Configuración del taglist
theme.taglist_fg_focus            = "#000000"
theme.taglist_bg_focus            = "#f6832f"
theme.taglist_bg_urgent           = "#f64a32"
theme.taglist_bg_occupied         = "#bedc87"
theme.taglist_fg_occupied         = "#000000"
theme.taglist_bg_empty            = "#000000"
theme.taglist_spacing             = dpi(2)
-- Configuración del tasklist
theme.tasklist_fg_normal          = "#000000"
theme.tasklist_bg_normal          = "#bedc87" 
theme.tasklist_fg_focus           = "#000000"
theme.tasklist_bg_focus           = "#f6832f"
theme.tasklist_fg_urgent          = "#000000"
theme.tasklist_bg_urgent          = "#f64a32"
theme.tasklist_shape              = gears.shape.rectangle
theme.tasklist_shape_border_color = "#000000"
theme.tasklist_shape_border_width = dpi(2)
theme.tasklist_spacing            = dpi(2)
theme.tasklist_plain_task_name    = false
theme.tasklist_disable_icon       = true
-- Iconos
theme.mini_icon                   = os.getenv("HOME") .. "/Imágenes/icon.png"
-- Padding de los clientes
theme.useless_gap                 = dpi(7)
-- Separador
local separator = wibox.widget.textbox('  ')

----------------------------------------------------------------------------------
-------------------------------- Widgets -----------------------------------------
----------------------------------------------------------------------------------
-- System Tray
local tray = wibox.widget{
  separator,
  wibox.widget.systray(),
  separator,
  layout = wibox.layout.fixed.horizontal
}
tray = wibox.container.background(tray, theme.applets_bg_1, gears.shape.rect)
tray = wibox.container.margin(tray, dpi(2), dpi(2), dpi(2), dpi(2))
tray = wibox.container.background(tray, theme.bg_normal, gears.shape.rect)
tray = wibox.container.margin(tray, dpi(1), dpi(2), dpi(2), dpi(2))

---- Batería
local batteryarc = batteryarc({ 
  font = theme.applets_font,
  bg_color = theme.applets_bg_2 }) 
batteryarc = wibox.container.background(batteryarc, theme.applets_bg_2, gears.shape.rect)
batteryarc = wibox.container.margin(batteryarc, dpi(2), dpi(2), dpi(2), dpi(2))
batteryarc = wibox.container.background(batteryarc, theme.bg_normal, gears.shape.rect)
batteryarc = wibox.container.margin(batteryarc, dpi(1), dpi(2), dpi(2), dpi(2))

-- Reloj
local mytextclock = wibox.widget.textclock(markup(theme.applets_fg, "  %H:%M " ))
mytextclock.font = theme.applets_font
local clockbg = wibox.container.background(mytextclock, theme.applets_bg_3, gears.rect)
local clockwidget = wibox.container.margin(clockbg, dpi(2), dpi(2), dpi(2), dpi(2))
clockwidget = wibox.container.background(clockwidget, theme.bg_normal, gears.shape.rect)
clockwidget = wibox.container.margin(clockwidget, dpi(1), dpi(2), dpi(2), dpi(2))

-- Fecha
local mytextcalendar = wibox.widget.textclock(markup.fontfg(theme.applets_font, theme.applets_fg, "  %a %d %b "))
local calbg = wibox.container.background(mytextcalendar, theme.applets_bg_1, gears.shape.rect)
calbg = wibox.container.margin(calbg, dpi(2), dpi(2), dpi(2), dpi(2))
calbg = wibox.container.background(calbg, theme.bg_normal, gears.shape.rect)

-- Calendario
-- TODO: cambiar el widget de lain a widgets, cambiar el color del icono a negro
--local calendarwidget = wibox.container.margin(calbg, dpi(1), dpi(2), dpi(2), dpi(2))
--theme.cal = lain.widget.cal({
--    attach_to = { mytextcalendar },
--    notification_preset = {
--        fg = theme.fg_normal,
--        bg = theme.applets_fg,
--        position = "top_right",
--        font = theme.applets_font
--    }
--})
local calendarwidget = wibox.container.margin(calbg, dpi(1), dpi(2), dpi(2), dpi(2))
theme.cal = calnot({
  attach_to = { mytextcalendar },
  notification_preset = {
    fg = theme.fg_normal,
    bg = theme.applets_fg,
    position = "top_right",
    font = theme.applets_font
  }
})
-- CPU
local cpu = lain.widget.cpu({
    settings = function()
        widget:set_markup( markup.fontfg(theme.applets_font, theme.applets_fg, "  CPU " .. cpu_now.usage.. "% "))
    end
})
local cpubg = wibox.container.background(cpu.widget, theme.applets_bg_2, gears.shape.rect)
local cpuwidget = wibox.container.margin(cpubg, dpi(2), dpi(2), dpi(2), dpi(2))
cpuwidget = wibox.container.background(cpuwidget, theme.bg_normal, gears.shape.rect)
cpuwidget = wibox.container.margin(cpuwidget, dpi(1), dpi(2), dpi(2), dpi(2))

-- Tráfico de red
local net = lain.widget.net({
    settings = function()
        widget:set_markup(markup.fontfg(theme.applets_font, theme.applets_fg, " " .. net_now.received .. "  -  " .. net_now.sent .. " "))
    end
})
local netbg = wibox.container.background(net.widget, theme.applets_bg_3, gears.shape.rect)
local networkwidget = wibox.container.margin(netbg, dpi(2), dpi(2), dpi(2), dpi(2))
networkwidget = wibox.container.background(networkwidget, theme.bg_normal, gears.shape.rect)
networkwidget = wibox.container.margin(networkwidget, dpi(1), dpi(2), dpi(2), dpi(2))



-- TODO: tratar de mover el widget a un archivo en la carpeta widget
-- Volumen
theme.volume = lain.widget.alsabar({
    notification_preset = { font = "Iosevka Custom 9"},
    -- togglechannel = "IEC958,3",
    width = dpi(100), height = dpi(8), border_width = dpi(0),
    colors = {
        background = theme.applets_bg_1,
        unmute     = theme.applets_fg,
        mute       = theme.applets_bg_1,
    },
})
theme.volume.bar.paddings = dpi(0)
theme.volume.bar.margins = dpi(5)
local volumewidget = wibox.container.background(theme.volume.bar, theme.applets_bg_1, gears.shape.rect)
volumewidget = wibox.container.margin(volumewidget, dpi(3), dpi(3), dpi(0), dpi(0))

-- Información multimedia
local songInfo = wibox.widget.textbox(markup.fontfg(theme.applets_font, theme.applets_fg,  " Nada en reprodución "))
songInfo = awful.widget.watch('sh ~/scripts/songInfo.sh', 1,
  function(songInfo, stdout, stderr, exitreason, exitcode)
    local f = io.popen('sh ~/scripts/songInfo.sh', 'r') 
    local l = f:read()
    songInfo:set_markup_silently(markup.fontfg(theme.applets_font, theme.applets_fg,  l))
    f:close()
  end, wibox.widget.textbox()
)
songInfo = wibox.container.margin(songInfo, dpi(0), dpi(0), dpi(2), dpi(2))

-- Botones de control multimedia
local prev_icon = wibox.widget.textbox(markup.fontfg(theme.applets_font, theme.applets_fg,  ""))
local next_icon = wibox.widget.textbox(markup.fontfg(theme.applets_font, theme.applets_fg,  ""))
local song_status_icon = wibox.widget.textbox(markup.fontfg(theme.applets_font, theme.applets_fg,  "  "))

song_status_icon = awful.widget.watch("playerctl status", 1,
  function(song_status_icon, stdout, stderr, exitreason, exitcode)
    local f = io.popen("playerctl status", 'r') 
    local l = f:read()
    if (l == "Playing") then
      song_status_icon:set_markup_silently(markup.fontfg(theme.applets_font, theme.applets_fg,  "  "))
    elseif (l == "Paused") then
      song_status_icon:set_markup(markup.fontfg(theme.applets_font, theme.applets_fg,  "  "))
    else
      song_status_icon:set_markup(markup.fontfg(theme.applets_font, theme.applets_fg,  "  "))
    end
    f:close()
  end, wibox.widget.textbox()
)

local player = wibox.widget{
  songInfo,
  prev_icon,
  song_status_icon,
  next_icon,
  volumewidget,
  layout = wibox.layout.fixed.horizontal
}

-- Funcionalidad de los botones de audio
prev_icon:connect_signal("button::press", function() os.execute("playerctl --player=playerctld previous") end)
song_status_icon:connect_signal("button::press", 
  function()
  os.execute("playerctl --player=playerctld play-pause")
  end)
next_icon:connect_signal("button::press", function() os.execute("playerctl --player=playerctld next") end)

-- Todo el widget de audio
local player_widget = wibox.container.background(player, theme.applets_bg_1, gears.shape.rect)
player_widget = wibox.container.margin(player_widget, dpi(2), dpi(2), dpi(2), dpi(2))
player_widget = wibox.container.background(player_widget, theme.bg_normal, gears.shape.rect)
player_widget = wibox.container.margin(player_widget, dpi(1), dpi(2), dpi(2), dpi(2))

-- Launcher
local mylauncher = awful.widget.button({ image = theme.mini_icon })
     -- TODO: que al dar click aaprrezca un menu de apagado, supensión, etc
-- mylauncher:connect_signal("button::press", function() awful.util.mymainmenu:toggle() end)

-- TODO: un widget de texto que marque que pantalla se está usando

function theme.at_screen_connect(s)

    -- Widget que muestra el layout actual
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(my_table.join(
                           awful.button({}, 1, function () awful.layout.inc( 1) end),
                           awful.button({}, 2, function () awful.layout.set( awful.layout.layouts[1] ) end),
                           awful.button({}, 3, function () awful.layout.inc(-1) end),
                           awful.button({}, 4, function () awful.layout.inc( 1) end),
                           awful.button({}, 5, function () awful.layout.inc(-1) end)))
    s.mylayoutbox = wibox.container.background(s.mylayoutbox, theme.bg_focus, gears.shape.rect)

    -- Widget que muestra las etiquetas, más configuración se encuentra en las lineas 52-56
    s.mytaglist = awful.widget.taglist{
      screen = s,
      filter = awful.widget.taglist.filter.all,
      buttons = awful.util.taglist_buttons}
    s.mytaglist = wibox.container.margin(s.mytaglist, dpi(2), dpi(0), dpi(2), dpi(2))
    s.mytaglistcont = wibox.container.background(s.mytaglist, theme.bg_focus, gears.shape.rect)

    -- Widget que contiene las etiquetas y el layout actual
    s.mytag = wibox.widget{s.mytaglistcont, separator, separator, s.mylayoutbox, separator, separator, layout = wibox.layout.fixed.horizontal}
    s.mytag = wibox.container.background(s.mytag, theme.bg_focus, gears.shape.rect)
    s.mytag = wibox.container.margin(s.mytag, dpi(6), dpi(3), dpi(2), dpi(2))

    -- Widget que muestra las aplicaciones en cada espacio
    mytasklist = awful.widget.tasklist(s,
                                        awful.widget.tasklist.filter.currenttags, 
                                        awful.util.tasklist_buttons 
                                        )
    s.mytasklist = wibox.container.margin(mytasklist, dpi(2), dpi(2), dpi(2), dpi(2))
    --s.mytasklist = wibox.container.background(s.mytasklist, theme.bg_focus, gears.shape.rect)
    --s.mytasklist = wibox.container.margin(s.mytasklist, dpi(3), dpi(3), dpi(2), dpi(2))

    -- Establece un wallpaper 
    local wallpaper = theme.wallpaper
    local wallpaper2 = theme.wallpaper2
    if type(wallpaper) == "function" then
        wallpaper = wallpaper(s)
    end

    -- Tags
    if s.index == 1 then
      awful.tag(awful.util.tagnames, s, awful.layout.layouts[2])
      gears.wallpaper.maximized(wallpaper, s)
      -- Barra superior
      s.mywibox = awful.wibar({ position = "top", screen = s, height = dpi(30), bg = "000000AA"})
      s.mywibox:setup {
          layout = wibox.layout.align.horizontal,
          { -- Parte izquierda 
              layout = wibox.layout.fixed.horizontal,
          },
          nil, -- Parte central
          { -- Parte derecha
              layout = wibox.layout.fixed.horizontal,
              player_widget,
              networkwidget,
              cpuwidget,
              calendarwidget,
              clockwidget,
              mybatterywidget,
              batteryarc,
              tray
          },
      }
    else
      awful.tag(awful.util.tagnames_sec, s, awful.layout.layouts[1])
      gears.wallpaper.maximized(wallpaper2, s)
    end


    -- Barra inferior 
    s.mybottomwibox = awful.wibar({ position = "bottom", screen = s, border_width = dpi(0), height = dpi(30), bg = "000000AA" })
    s.mybottomwibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Parte izquierda
            layout = wibox.layout.fixed.horizontal,
            mylauncher,
            s.mytag,
        },
        s.mytasklist, -- Parte central
        { -- Parte derecha
            layout = wibox.layout.fixed.horizontal,
        },
    }
end

return theme
-- TODO cambiar el tema de rofi a uno de github text
