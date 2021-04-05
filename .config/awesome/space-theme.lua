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
local awful = require("awful")
local wibox = require("wibox")
local dpi   = require("beautiful.xresources").apply_dpi
local my_table   = awful.util.table or gears.table
local watch      = require("awful.widget.watch")
local batteryarc = require("widgets.batteryarc")
local calnot     = require("widgets.cal")
local wcpu       = require("widgets.cpu")
local wnet       = require("widgets.net")
local alsabar    = require("widgets.alsabar")
local player     = require("widgets.player")
local markup     = require("markup")

----------------------------------------------------------------------------------
---------------------------- Definición de varibles ------------------------------
----------------------------------------------------------------------------------
local theme                       = {}
-- Directorios por default
theme.default_dir                 = require("awful.util").get_themes_dir() .. "default"
-- theme.wallpaper                   = os.getenv("HOME") .. "/Imágenes/fondo2.png"
theme.wallpaper                   = os.getenv("HOME") .. "/Imágenes/suitsat1_nasa_2008.jpg"
theme.wallpaper2                  = os.getenv("HOME") .. "/Imágenes/suitsat1_nasa_2008.jpg"
-- Fuentes
theme.font                        = "JetBrainsMono Nerd Font 9"
theme.player_font                 = "Hurmit Nerd Font Mono 10"
theme.taglist_font                = "JetBrainsMono Nerd Font Bold 10"
-- Colores
theme.fg_normal                   = "#FFFFFF"
theme.bg_normal                   = "#000000"
theme.fg_focus                    = "#79E7FF"
theme.bg_focus                    = "#000000"
theme.fg_urgent                   = "#000000"
theme.bg_urgent                   = "#f64a32"
-- Colores de los applets
theme.applets_font                 = "JetBrainsMono Nerd Font 10"
theme.applets_fg                  = "#FFFFFF"
theme.applets_bg_1                = "#000000"
theme.applets_bg_2                = "#000000"
theme.applets_bg_3                = "#000000"
theme.bg_systray                  = theme.applets_bg_1
theme.systray_icon_spacing        = dpi(2)
theme.applets_spacing             = dpi(2)
theme.fg_player                   = "#bedc87"
-- Bordes de clientes
theme.border_width                = dpi(2)
theme.border_normal               = "#000000"
theme.border_focus                = "#FFFFFF"
-- Configuración del taglist
theme.taglist_fg_focus            = "#000000"
theme.taglist_bg_focus            = "#79E7FF"
theme.taglist_fg_occupied         = "#000000"
theme.taglist_bg_occupied         = "#FFFFFF"
theme.taglist_bg_empty            = "#000000"
theme.taglist_bg_urgent           = "#f64a32"
theme.taglist_spacing             = dpi(4)
-- Configuración del tasklist
theme.tasklist_fg_normal          = "#000000"
theme.tasklist_bg_normal          = "#FFFFFF"
theme.tasklist_fg_focus           = "#000000"
theme.tasklist_bg_focus           = "#79E7FF"
theme.tasklist_fg_urgent          = "#000000"
theme.tasklist_bg_urgent          = "#f64a32"
theme.tasklist_shape              = gears.shape.rectangle
theme.tasklist_shape_border_color = "#FFFFFF"
theme.tasklist_shape_border_width = dpi(0)
theme.tasklist_spacing            = dpi(4)
theme.tasklist_plain_task_name    = false
theme.tasklist_disable_icon       = false
-- Iconos
theme.mini_icon                   = os.getenv("HOME") .. "/Imágenes/icon.png"
-- Padding de los clientes
theme.useless_gap                 = dpi(4)
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
mytextclock.refresh = 20
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
local calendarwidget = wibox.container.margin(calbg, dpi(1), dpi(2), dpi(2), dpi(2))
theme.cal = calnot({
  attach_to = { mytextcalendar },
  notification_preset = {
    fg = theme.fg_normal,
    bg = theme.bg_normal,
    position = "top_right",
    font = theme.applets_font
  }
})

-- CPU
local cpu = wcpu({
  settings = function()
    widget:set_markup( markup.fontfg(theme.applets_font, theme.applets_fg, "  CPU " .. cpu_now.usage.. "% "))
  end
})
local cpubg = wibox.container.background(cpu.widget, theme.applets_bg_2, gears.shape.rect)
local cpuwidget = wibox.container.margin(cpubg, dpi(2), dpi(2), dpi(2), dpi(2))
cpuwidget = wibox.container.background(cpuwidget, theme.bg_normal, gears.shape.rect)
cpuwidget = wibox.container.margin(cpuwidget, dpi(1), dpi(2), dpi(2), dpi(2))

-- Tráfico de red
local net = wnet({
  settings = function()
    widget:set_markup(markup.fontfg(theme.applets_font, theme.applets_fg, " " .. net_now.received .. "  -  " .. net_now.sent .. " "))
  end
})
local netbg = wibox.container.background(net.widget, theme.applets_bg_3, gears.shape.rect)
local networkwidget = wibox.container.margin(netbg, dpi(2), dpi(2), dpi(2), dpi(2))
networkwidget = wibox.container.background(networkwidget, theme.bg_normal, gears.shape.rect)
networkwidget = wibox.container.margin(networkwidget, dpi(1), dpi(2), dpi(2), dpi(2))

-- -- Volumen
-- theme.volume = alsabar({
--     notification_preset = { font = "Iosevka Custom 9"},
--     -- togglechannel = "IEC958,3",
--     width = dpi(100), height = dpi(8), border_width = dpi(0),
--     colors = {
--         background = theme.applets_bg_1,
--         unmute     = theme.applets_fg,
--         mute       = theme.applets_bg_1,
--     },
-- })
-- theme.volume.bar.paddings = dpi(0)
-- theme.volume.bar.margins = dpi(5)
-- local volumewidget = wibox.container.background(theme.volume.bar, theme.applets_bg_1, gears.shape.rect)
-- volumewidget = wibox.container.margin(volumewidget, dpi(3), dpi(3), dpi(0), dpi(0))

-- -- Todo el widget de audio
-- local wplayer = player()
-- local wplayer = wibox.widget{
--   wplayer,
--   volumewidget,
--   layout = wibox.layout.fixed.horizontal
-- } 
-- local player_widget = wibox.container.background(wplayer, theme.applets_bg_1, gears.shape.rect)
-- player_widget = wibox.container.margin(player_widget, dpi(2), dpi(2), dpi(2), dpi(2))
-- player_widget = wibox.container.background(player_widget, theme.bg_normal, gears.shape.rect)
-- player_widget = wibox.container.margin(player_widget, dpi(1), dpi(2), dpi(2), dpi(2))

-- Launcher
local mylauncher = awful.widget.button({ image = theme.mini_icon })

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
