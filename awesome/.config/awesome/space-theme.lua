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
local dpi = require("beautiful.xresources").apply_dpi
local my_table = awful.util.table or gears.table
local watch = require("awful.widget.watch")
local batteryarc = require("widgets.batteryarc")
local calnot = require("widgets.cal")
local wcpu = require("widgets.cpu")
local wnet = require("widgets.net")
local alsabar = require("widgets.alsabar")
local player = require("widgets.player")
local volume_widget = require('awesome-wm-widgets.volume-widget.volume')
local markup = require("markup")
local naughty = require("naughty")

-- ----------------------------
-- -- Definición de varibles --
-- ----------------------------
local theme = {}

-- Directorios por default
theme.default_dir = require("awful.util").get_themes_dir() .. "default"
theme.wallpaper = os.getenv("HOME") .. "/.config/awesome/wallpapers/mountain2.jpg"
-- theme.wallpaper = os.getenv("HOME") .. "/.config/awesome/wallpapers/f2.jpg"
-- theme.wallpaper = os.getenv("HOME") .. "/.config/awesome/wallpapers/faye.jpg"
-- theme.wallpaper = os.getenv("HOME") .. "/.config/awesome/wallpapers/seeyouspacecowboy.png"
-- theme.wallpaper = os.getenv("HOME") .. "/Pictures/suitsat1_nasa_2008.jpg"
-- theme.wallpaper = os.getenv("HOME") .. "/Imágenes/wallpaper1.jpg"
-- theme.wallpaper2 = os.getenv("HOME") .. "/Pictures/f2.jpg"
theme.wallpaper2 = os.getenv("HOME") .. "/.config/awesome/wallpapers/f2.jpg"

-- Fuentes
theme.font = "JetBrainsMono Nerd Font Regular 12"
theme.taglist_font = theme.font
theme.prompt_font = theme.font
theme.player_font = theme.font
-- theme.taglist_font = "JetBrainsMono Nerd Font Regular 12"
-- theme.prompt_font = "JetBrainsMono Nerd Font 9"
-- theme.player_font = "Hurmit Nerd Font Mono 10"
-- theme.font = "Iosevka 12"
-- theme.prompt_font = "Iosevka 9"
-- theme.player_font = "Iosevka 10"
-- theme.taglist_font = "Iosevka Italic 12"

-- Colores
theme.color1 = "#e2e2eb" -- Claro
theme.color2 = "#000000" -- Oscuro
theme.color3 = "#ff7b00" -- Color de acento
theme.color4 = "#f64a32" -- Rojo, o algún color de urgente
theme.color5 = "#0000000" -- Color transparente

-- Aplicación de colores
theme.fg_normal = theme.color1
theme.bg_normal = theme.color2
theme.fg_focus = theme.color1
theme.bg_focus = theme.color2
theme.fg_urgent = theme.color2
theme.bg_urgent = theme.color4

-- Colores de los applets
theme.applets_font = theme.font
theme.applets_fg = theme.color1
theme.applets_bg = theme.color2
theme.applets_spacing = dpi(2)

-- Systray
theme.bg_systray = theme.applets_bg
theme.systray_icon_spacing = dpi(2)

-- Bordes de clientes
theme.border_width = dpi(2)
theme.border_normal = theme.color1
theme.border_focus = theme.color3

-- Configuración del taglist
theme.taglist_fg_empty = theme.color1
theme.taglist_bg_empty = theme.color2
theme.taglist_fg_occupied = theme.color2
theme.taglist_bg_occupied = theme.color1
theme.taglist_fg_focus = theme.color1
theme.taglist_bg_focus = theme.color5
theme.taglist_shape = gears.shape.rectangle
theme.taglist_shape_border_color_focus = theme.color1
theme.taglist_shape_border_width_focus = dpi(2)
theme.taglist_bg_urgent = theme.color4
theme.taglist_spacing = dpi(4)

-- Configuración del tasklist
theme.tasklist_shape = gears.shape.rectangle
theme.tasklist_disable_icon = true
theme.tasklist_fg_normal = theme.color1
theme.tasklist_bg_normal = theme.color2
theme.tasklist_fg_focus = theme.color1
theme.tasklist_bg_focus = theme.color5
theme.tasklist_fg_urgent = theme.color1
theme.tasklist_bg_urgent = theme.color4
theme.tasklist_shape_border_color_focus = theme.color1
theme.tasklist_shape_border_width_focus = dpi(3)
theme.tasklist_spacing = dpi(4)
theme.tasklist_plain_task_name = false

-- Iconos
-- theme.mini_icon = os.getenv("HOME") .. "/Imágenes/think.png"
theme.mini_icon = os.getenv("HOME") .. "/Pictures/icon.png"

-- Padding de los clientes
theme.useless_gap = dpi(2)

-- Prompts
theme.prompt_fg = theme.color1
theme.prompt_bg = theme.color2

-- Separador
local separator = wibox.widget.textbox('  ')

-- -------------
-- -- Widgets --
-- -------------
-- System Tray
local tray = wibox.widget{
  separator,
  wibox.widget.systray(),
  separator,
  layout = wibox.layout.fixed.horizontal
}
tray = wibox.container.background(tray, theme.applets_bg, gears.shape.rect)
tray = wibox.container.margin(tray, dpi(3), dpi(3), dpi(3), dpi(3))
tray = wibox.container.background(tray, theme.bg_normal, gears.shape.rect)
tray = wibox.container.margin(tray, dpi(0), dpi(2), dpi(0), dpi(0))

-- Batería
local batteryarc = batteryarc({
  font = theme.applets_font,
  bg_color = theme.applets_bg})
batteryarc = wibox.container.background(batteryarc, theme.applets_bg, gears.shape.rect)
-- batteryarc = wibox.container.margin(batteryarc, dpi(2), dpi(2), dpi(2), dpi(2))
batteryarc = wibox.container.background(batteryarc, theme.bg_normal, gears.shape.rect)
batteryarc = wibox.container.margin(batteryarc, dpi(2), dpi(2), dpi(0), dpi(0))

-- Reloj
local mytextclock = wibox.widget.textclock(markup(theme.applets_fg, "  %H:%M "))
mytextclock.font = theme.applets_font
mytextclock.refresh = 20
local clockbg = wibox.container.background(mytextclock, theme.applets_bg, gears.rect)
local clockwidget = wibox.container.background(clockbg, theme.bg_normal, gears.shape.rect)
clockwidget = wibox.container.margin(clockwidget, dpi(2), dpi(2), dpi(0), dpi(0))

-- Fecha
local mytextcalendar = wibox.widget.textclock(markup.fontfg(theme.applets_font, theme.applets_fg, "   %a %d %b "))
local calbg = wibox.container.background(mytextcalendar, theme.applets_bg, gears.shape.rect)
calbg = wibox.container.background(calbg, theme.bg_normal, gears.shape.rect)
-- calbg = wibox.container.margin(calbg, dpi(2), dpi(2), dpi(0), dpi(0))

-- Calendario
local calendarwidget = wibox.container.margin(calbg, dpi(2), dpi(2), dpi(0), dpi(0))
theme.cal = calnot({
		attach_to = { mytextcalendar },
		notification_preset = {
			fg = theme.fg_normal,
			bg = theme.bg_normal,
			position = "top_left",
			font = theme.applets_font
		}
})

-- CPU
local cpu = wcpu({
		settings = function()
		widget:set_markup(
			markup.fontfg(theme.applets_font, theme.applets_fg, "  CPU " .. cpu_now.usage.. "% "))
		end
})
local cpubg = wibox.container.background(cpu.widget, theme.applets_bg, gears.shape.rect)
local cpuwidget = wibox.container.margin(cpubg, dpi(2), dpi(2), dpi(0), dpi(0))
-- cpuwidget = wibox.container.background(cpuwidget, theme.bg_normal, gears.shape.rect)
-- cpuwidget = wibox.container.margin(cpuwidget, dpi(1), dpi(2), dpi(2), dpi(2))

-- Tráfico de red
local net = wnet({
		settings = function()
		widget:set_markup(
			markup.fontfg(theme.applets_font, theme.applets_fg, " " .. net_now.received .. " ﲐ - ﲓ ".. net_now.sent .. " "))
		end
})
local netbg = wibox.container.background(net.widget, theme.applets_bg, gears.shape.rect)
local networkwidget = wibox.container.margin(netbg, dpi(2), dpi(2), dpi(0), dpi(0))
-- networkwidget = wibox.container.background(networkwidget, theme.bg_normal, gears.shape.rect)
-- networkwidget = wibox.container.margin(networkwidget, dpi(1), dpi(2), dpi(2), dpi(2))

-- Launcher
local mylauncher = awful.widget.button({ image = theme.mini_icon })
mylauncher = wibox.container.margin(mylauncher, dpi(4), dpi(4), dpi(3), dpi(0))

-- Función que se encarga de generar los elementos de las pantallas
function theme.at_screen_connect(s)
	-- Todos los widgets en uno solo
	s.mywidgets = wibox.widget{
		tray,
		batteryarc,
		networkwidget,
		cpuwidget,
		calendarwidget,
		mycountdownwidget,
		clockwidget,
		volumen,
		mybatterywidget,
		layout = wibox.layout.fixed.horizontal}
	s.mywidgets = wibox.container.margin(s.mywidgets, dpi(0), dpi(0), dpi(0), dpi(2))

	-- Widget que muestra el layout actual
	s.mylayoutbox = wibox.widget{
		separator,
		awful.widget.layoutbox(s),
		separator,
		layout = wibox.layout.fixed.horizontal}
	s.mylayoutbox:buttons(my_table.join(
			awful.button({}, 1, function () awful.layout.inc( 1) end),
			awful.button({}, 2, function () awful.layout.set( awful.layout.layouts[1] ) end),
			awful.button({}, 3, function () awful.layout.inc(-1) end),
			awful.button({}, 4, function () awful.layout.inc( 1) end),
			awful.button({}, 5, function () awful.layout.inc(-1) end)))
	s.mylayoutbox = wibox.container.background(s.mylayoutbox, theme.bg_focus, gears.shape.rect)
	s.mylayoutbox = wibox.container.margin(s.mylayoutbox, dpi(4), dpi(4), dpi(0), dpi(2))

	-- Widget que muestra las etiquetas, más configuración se encuentra en las lineas 52-56
	s.mytaglist = awful.widget.taglist{
		screen = s,
		filter = awful.widget.taglist.filter.all,
		buttons = awful.util.taglist_buttons}
	s.mytaglist = wibox.container.margin(s.mytaglist, dpi(4), dpi(4), dpi(3), dpi(0))
	-- Widget que contiene las etiquetas
	s.mytag = wibox.widget{
		s.mytaglist,
		layout = wibox.layout.fixed.horizontal}

	s.mytasklist = awful.widget.tasklist(s,
		awful.widget.tasklist.filter.currenttags,
		awful.util.tasklist_buttons
	)
	s.mytasklist = wibox.container.margin(s.mytasklist, dpi(4), dpi(4), dpi(3), dpi(0))

	-- Establece un wallpaper
	local wallpaper = theme.wallpaper
	local wallpaper2 = theme.wallpaper2
	if type(wallpaper) == "function" then
		wallpaper = wallpaper(s)
	end

	-- Configuraciones especiales para el monitor principal
	if s.index == 1 then
		-- Define el layout de los clientes por defecto
		awful.tag(awful.util.tagnames, s, awful.layout.layouts[2])
		-- Establece el wallpaper
		gears.wallpaper.maximized(wallpaper, s)
		-- Elementos de la Barra superior
		s.mywibox = awful.wibar({ position = "top", screen = s, height = dpi(30), bg = "000000"})
		s.mywibox:setup {
			layout = wibox.layout.align.horizontal,
			{ -- Parte izquierda
				layout = wibox.layout.fixed.horizontal,
				s.mylayoutbox,
				s.mywidgets
			},
			nil, -- Parte central
			{ -- Parte derecha
				layout = wibox.layout.fixed.horizontal,
			},
		}
	-- Configuraciones para las demás pantallas
	else
		-- Layout de los clientes por defecto
		awful.tag(awful.util.tagnames_sec, s, awful.layout.layouts[2])
		-- Establece el wallpaper
		gears.wallpaper.maximized(wallpaper2, s)
	end

	-- Barra inferior
	s.mybottomwibox = awful.wibar({ position = "bottom", screen = s, border_width = dpi(0), height = dpi(30), bg = "000000AA" })
	s.mybottomwibox:setup {
		layout = wibox.layout.align.horizontal,
		{ -- Parte izquierda
			s.mytag,
			mylauncher,
			layout = wibox.layout.fixed.horizontal,
		},
		s.mytasklist, -- Parte central
		{ -- Parte derecha
			layout = wibox.layout.fixed.horizontal,
		},
	}
end

return theme
-- DONE: TODO: definir una lista de colores y de ahí tomar los colores para los demás elementos
