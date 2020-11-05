--[[
     Configuración de AwesomeWM de github.com/luis-barrera
     Inspirado por:
     Awesome WM configuration template en github.com/lcpz
--]]

----------------------------------------------------------------------------------
------------------------------ Librerias necesarias ------------------------------
----------------------------------------------------------------------------------
local gears = require("gears")
local lain  = require("lain")
local awful = require("awful")
local wibox = require("wibox")
local dpi   = require("beautiful.xresources").apply_dpi
local string, os = string, os
local my_table = awful.util.table or gears.table -- 4.{0,1} compatibility

-- necesario clonar el repositorio de github github.com/streetturtle/awesome-wm-widgets
local battery_widget = require("awesome-wm-widgets.battery-widget.battery")

----------------------------------------------------------------------------------
---------------------------- Definición de varibles ------------------------------
----------------------------------------------------------------------------------
local theme                                     = {}
-- Directorios por default
theme.default_dir                               = require("awful.util").get_themes_dir() .. "default"
theme.icon_dir                                  = os.getenv("HOME") .. "/.config/awesome/icons"
theme.wallpaper                                 = os.getenv("HOME") .. "/Imágenes/fondo2.png"
theme.wallpaper2                                = os.getenv("HOME") .. "/Imágenes/f2.jpg"
-- Fuentes
theme.font                                      = "Source Code Pro 9"
theme.taglist_font                              = "Source Code Pro Bold 10"
-- Colores
theme.fg_normal                                 = "#FFFFFF"
theme.fg_focus                                  = "#f6832f"
theme.bg_focus                                  = "#303030"
theme.bg_normal                                 = "#242424"
theme.fg_urgent                                 = "#f8ccae"
theme.bg_urgent                                 = "#f64a32"
theme.bg_systray                                = "#303030"
-- Bordes de clientes
theme.border_width                              = dpi(2)
theme.border_normal                             = "#252525"
theme.border_focus                              = "#f6832f"
theme.taglist_fg_focus                          = "#FFFFFF"
theme.tasklist_bg_normal                        = "#222222"
theme.tasklist_fg_focus                         = "#FFFFFF"
-- Propiedades del menu
theme.menu_height                               = dpi(20)
theme.menu_width                                = dpi(160)
theme.menu_icon_size                            = dpi(30)
-- Iconos
theme.awesome_icon                              = theme.icon_dir .. "/awesome_icon_white.png"
theme.awesome_icon_launcher                     = theme.icon_dir .. "/awesome_icon.png"
theme.taglist_squares_sel                       = theme.icon_dir .. "/square_sel.png"
theme.taglist_squares_unsel                     = theme.icon_dir .. "/square_unsel.png"
theme.spr_small                                 = theme.icon_dir .. "/spr_small.png"
theme.spr_very_small                            = theme.icon_dir .. "/spr_very_small.png"
theme.spr_right                                 = theme.icon_dir .. "/spr_right.png"
theme.spr_bottom_right                          = theme.icon_dir .. "/spr_bottom_right.png"
theme.spr_left                                  = theme.icon_dir .. "/spr_left.png"
theme.bar                                       = theme.icon_dir .. "/bar.png"
theme.bottom_bar                                = theme.icon_dir .. "/bottom_bar.png"
theme.mpdl                                      = theme.icon_dir .. "/mpd.png"
theme.mpd_on                                    = theme.icon_dir .. "/mpd_on.png"
theme.prev                                      = theme.icon_dir .. "/prev.png"
theme.nex                                       = theme.icon_dir .. "/next.png"
theme.stop                                      = theme.icon_dir .. "/stop.png"
theme.pause                                     = theme.icon_dir .. "/pause.png"
theme.play                                      = theme.icon_dir .. "/play.png"
theme.clock                                     = theme.icon_dir .. "/clock.png"
theme.calendar                                  = theme.icon_dir .. "/cal.png"
theme.cpu                                       = theme.icon_dir .. "/cpu.png"
theme.net_up                                    = theme.icon_dir .. "/net_up.png"
theme.net_down                                  = theme.icon_dir .. "/net_down.png"
theme.layout_tile                               = theme.icon_dir .. "/tile.png"
theme.layout_tileleft                           = theme.icon_dir .. "/tileleft.png"
theme.layout_tilebottom                         = theme.icon_dir .. "/tilebottom.png"
theme.layout_tiletop                            = theme.icon_dir .. "/tiletop.png"
theme.layout_fairv                              = theme.icon_dir .. "/fairv.png"
theme.layout_fairh                              = theme.icon_dir .. "/fairh.png"
theme.layout_spiral                             = theme.icon_dir .. "/spiral.png"
theme.layout_dwindle                            = theme.icon_dir .. "/dwindle.png"
theme.layout_max                                = theme.icon_dir .. "/max.png"
theme.layout_fullscreen                         = theme.icon_dir .. "/fullscreen.png"
theme.layout_magnifier                          = theme.icon_dir .. "/magnifier.png"
theme.layout_floating                           = theme.icon_dir .. "/floating.png"
theme.mini_icon                                 = theme.icon_dir .. "/icon.png"
-- Propiedades del tasklist
theme.tasklist_plain_task_name                  = false
theme.tasklist_disable_icon                     = true
-- Padding de los clientes
theme.useless_gap                               = dpi(2)
-- Iconos de la barra de título
theme.titlebar_close_button_normal              = theme.default_dir.."/titlebar/close_normal.png"
theme.titlebar_close_button_focus               = theme.default_dir.."/titlebar/close_focus.png"
theme.titlebar_minimize_button_normal           = theme.default_dir.."/titlebar/minimize_normal.png"
theme.titlebar_minimize_button_focus            = theme.default_dir.."/titlebar/minimize_focus.png"
theme.titlebar_ontop_button_normal_inactive     = theme.default_dir.."/titlebar/ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive      = theme.default_dir.."/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active       = theme.default_dir.."/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active        = theme.default_dir.."/titlebar/ontop_focus_active.png"
theme.titlebar_sticky_button_normal_inactive    = theme.default_dir.."/titlebar/sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive     = theme.default_dir.."/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active      = theme.default_dir.."/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active       = theme.default_dir.."/titlebar/sticky_focus_active.png"
theme.titlebar_floating_button_normal_inactive  = theme.default_dir.."/titlebar/floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive   = theme.default_dir.."/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active    = theme.default_dir.."/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_active     = theme.default_dir.."/titlebar/floating_focus_active.png"
theme.titlebar_maximized_button_normal_inactive = theme.default_dir.."/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive  = theme.default_dir.."/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active   = theme.default_dir.."/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active    = theme.default_dir.."/titlebar/maximized_focus_active.png"


local markup = lain.util.markup
local blue   = "#80CCE6"
local space3 = markup.font("Roboto 3", " ")

-- Clock
local mytextclock = wibox.widget.textclock(markup("#FFFFFF", space3 .. "   %H:%M  " .. markup.font("Roboto 4", " ")))
mytextclock.font = theme.font
local clockbg = wibox.container.background(mytextclock, theme.bg_focus, gears.shape.rounded_bar)
local clockwidget = wibox.container.margin(clockbg, dpi(3), dpi(3), dpi(1), dpi(1))

-- Calendar
local mytextcalendar = wibox.widget.textclock(markup.fontfg(theme.font, "#FFFFFF", space3 .. "   %a %d %b  " .. markup.font("Roboto 5", " ")))
local calbg = wibox.container.background(mytextcalendar, theme.bg_focus, gears.shape.rounded_bar)
local calendarwidget = wibox.container.margin(calbg, dpi(3), dpi(3), dpi(1), dpi(1))
theme.cal = lain.widget.cal({
    attach_to = { mytextclock, mytextcalendar },
    notification_preset = {
        fg = "#FFFFFF",
        bg = theme.bg_normal,
        position = "bottom_right",
        font = "Source Code Pro 10"
    }
})

-- ALSA volume bar
theme.volume = lain.widget.alsabar({
    notification_preset = { font = "Source Code Pro 9"},
    --togglechannel = "IEC958,3",
    width = dpi(100), height = dpi(10), border_width = dpi(0),
    colors = {
        background = "#383838",
        unmute     = "#80CCE6",
        mute       = "#FF9F9F"
    },
})
theme.volume.bar.paddings = dpi(0)
theme.volume.bar.margins = dpi(5)
local volumewidget = wibox.container.background(theme.volume.bar, theme.bg_focus, gears.shape.rounded_bar)
volumewidget = wibox.container.margin(volumewidget, dpi(3), dpi(3), dpi(1), dpi(1))

-- Music player
local songInfo = wibox.widget.textbox('')
songInfo = awful.widget.watch('sh ~/.config/awesome/songInfo.sh', 1,
  function(songInfo, stdout, stderr, exitreason, exitcode)
    local f = io.popen('sh ~/.config/awesome/songInfo.sh', 'r') 
    local l = f:read()
    songInfo:set_markup_silently(l)
    f:close()
  end, wibox.widget.textbox()
)
songInfo = wibox.container.margin(songInfo, dpi(3), dpi(3), dpi(2), dpi(1))
local player_icon = wibox.widget.imagebox(theme.mpdl, gears.shape.rounded_bar)
local prev_icon = wibox.widget.imagebox(theme.prev, gears.shape.rounded_bar)
local next_icon = wibox.widget.imagebox(theme.nex, gears.shape.rounded_bar)
local stop_icon = wibox.widget.imagebox(theme.stop, gears.shape.rounded_bar)
local pause_icon = wibox.widget.imagebox(theme.pause, gears.shape.rounded_bar)
local play_pause_icon = wibox.widget.imagebox(theme.stop, gears.shape.rounded_bar)
play_pause_icon = awful.widget.watch('playerctl status', 1, 
  function(play_pause_icon, stdout, stderr, exitreason, exitcode)
    local f = io.popen('playerctl status', 'r') 
    local l = f:read()
    if (l == "Playing") then
      play_pause_icon:set_image(theme.pause)
    elseif (l == "Paused") then
      play_pause_icon:set_image(theme.play)
    else
      play_pause_icon:set_image(theme.stop)
    end
    f:close()
  end, wibox.widget.imagebox()
)
local separator = wibox.widget.textbox('  ')
local player = wibox.widget{separator,
                            songInfo,
                            separator,
                            prev_icon,
                            play_pause_icon,
                            next_icon,
                            volumewidget,
                            separator,
                            layout = wibox.layout.fixed.horizontal
                            }

-- Todo el widget de audio
local player_widget = wibox.container.background(player, theme.bg_focus, gears.shape.rounded_bar)
player_widget = wibox.container.margin(player_widget, dpi(3), dpi(3), dpi(1), dpi(1))

-- Song description
local function playerStatus()
  
end

-- Funcionalidad de los botones de audio
prev_icon:connect_signal("button::press", function() os.execute("playerctl --player=playerctld previous") end)
play_pause_icon:connect_signal("button::press", 
  function()
  os.execute("playerctl --player=playerctld play-pause")
  end)
next_icon:connect_signal("button::press", function() os.execute("playerctl --player=playerctld next") end)

-- battery
local mybatterywidget = battery_widget({display_notification = true, timeout = 1, show_current_level = true});
mybatterywidget = wibox.container.margin(mybatterywidget, dpi(3), dpi(3), dpi(3), dpi(1))

-- Notificación sobre precio de minecraft
local price_checker = wibox.widget.textbox('')
price_checker = awful.widget.watch('python ~/linux-dotfiles/price-tracker/minecraft-price-tracker.py', 1800,
  function(price_checker, stdout, stderr, exitreason, exitcode)
    local f = io.popen('python ~/linux-dotfiles/price-tracker/minecraft-price-tracker.py', 'r') 
    local l = f:read()
    price_checker:set_markup_silently(l)
    f:close()
  end, wibox.widget.textbox()
)

-- System Tray icons en un widget
local tray = wibox.widget{separator,
                          separator,
                          price_checker,
                          separator,
                          separator,
                          mybatterywidget,
                          wibox.widget.systray(),
                          separator,
                          separator,
                          layout = wibox.layout.fixed.horizontal
                          }
tray = wibox.container.background(tray, theme.bg_focus, gears.shape.rounded_bar)
tray = wibox.container.margin(tray, dpi(3), dpi(3), dpi(1), dpi(1))

-- CPU
local cpu = lain.widget.cpu({
    settings = function()
        widget:set_markup(space3 .. markup.font(theme.font, "   CPU " .. cpu_now.usage
                          .. "%  ") .. markup.font("Roboto 5", " "))
    end
})
local cpubg = wibox.container.background(cpu.widget, theme.bg_focus, gears.shape.rounded_bar)
local cpuwidget = wibox.container.margin(cpubg, dpi(3), dpi(3), dpi(1), dpi(1))

-- Net
local netdown_icon = wibox.widget.imagebox(theme.net_down)
local netup_icon = wibox.widget.imagebox(theme.net_up)
local net = lain.widget.net({
    settings = function()
        widget:set_markup(markup.font("Roboto 1", " ") .. markup.font(theme.font, "  " .. net_now.received .. "  -  "
                          .. net_now.sent .. "  ") .. markup.font("Roboto 2", " "))
    end
})
local netbg = wibox.container.background(net.widget, theme.bg_focus, gears.shape.rounded_bar)
local networkwidget = wibox.container.margin(netbg, dpi(3), dpi(3), dpi(1), dpi(1))

-- Launcher
local mylauncher = awful.widget.button({ image = theme.mini_icon })
-- mylauncher:connect_signal("button::press", function() awful.util.mymainmenu:toggle() end)

local barcolor  = gears.color({
    type  = "linear",
    from  = { dpi(32), 0 },
    to    = { dpi(32), dpi(32) },
    stops = { {0, theme.bg_focus}, {0.25, "#505050"}, {1, theme.bg_focus} }
})

-- Widget que muestra la pantalla activa
--local s_act = awful.screen.focused()
--local screen_active_widget = wibox.widget.textbox("pantalla activa")


function theme.at_screen_connect(s)
    -- Establece un wallpaper 
    local wallpaper = theme.wallpaper
    local wallpaper2 = theme.wallpaper2
    if type(wallpaper) == "function" then
        wallpaper = wallpaper(s)
    end

    -- Tags
    if s.index == 1 then
      awful.tag(awful.util.tagnames, s, awful.layout.layouts[1])
      gears.wallpaper.maximized(wallpaper, s)
    else
      awful.tag(awful.util.tagnames_sec, s, awful.layout.layouts[1])
      gears.wallpaper.maximized(wallpaper2, s)
    end
    
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(my_table.join(
                           awful.button({}, 1, function () awful.layout.inc( 1) end),
                           awful.button({}, 2, function () awful.layout.set( awful.layout.layouts[1] ) end),
                           awful.button({}, 3, function () awful.layout.inc(-1) end),
                           awful.button({}, 4, function () awful.layout.inc( 1) end),
                           awful.button({}, 5, function () awful.layout.inc(-1) end)))
    s.mylayoutbox = wibox.container.background(s.mylayoutbox, theme.bg_focus, gears.shape.rounded_bar)
    s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all, awful.util.taglist_buttons, { bg_focus = barcolor })
    s.mytaglist = wibox.container.margin(s.mytaglist, dpi(9), dpi(9), dpi(1), dpi(1))
    s.mytaglistcont = wibox.container.background(s.mytaglist, theme.bg_focus, gears.shape.rounded_bar)
    s.mytag = wibox.widget{separator, s.mytaglistcont, s.mylayoutbox, screen_active_widget, separator, separator, layout = wibox.layout.fixed.horizontal}
    s.mytag = wibox.container.background(s.mytag, theme.bg_focus, gears.shape.rounded_bar)
    s.mytag = wibox.container.margin(s.mytag, dpi(3), dpi(3), dpi(1), dpi(1))

    -- Create a tasklist widget
    mytasklist = awful.widget.tasklist(s,
                                        awful.widget.tasklist.filter.currenttags, 
                                        awful.util.tasklist_buttons, 
                                        -- layout = {spacing = 10},
                                        { bg_focus = theme.bg_focus, 
                                          shape = gears.shape.rounded_bar, 
                                          shape_border_width = 5, 
                                          shape_border_color = theme.tasklist_bg_normal, 
                                          align = "center" 
                                        })
    s.mytasklist = wibox.container.margin(mytasklist, dpi(6), dpi(3), dpi(1), dpi(1))

    -- Create the wibox
    s.mywibox = awful.wibar({ position = "top", screen = s, height = dpi(30), bg = "000000AA"})
    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            s.mytag,
            --  s.mypromptbox
        },
        nil, -- Middle widget
        { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            player_widget,
            tray
        },
    }

    -- Create the bottom wibox
    s.mybottomwibox = awful.wibar({ position = "bottom", screen = s, border_width = dpi(0), height = dpi(28), bg = "000000AA" })
    -- Add widgets to the bottom wibox
    s.mybottomwibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            mylauncher,
        },
        s.mytasklist, -- Middle widget
        { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            networkwidget,
            cpuwidget,
            calendarwidget,
            clockwidget,
        },
    }
end
--[[

theme.mpd = lain.widget.mpd({
    settings = function ()
        if mpd_now.state == "play" then
            mpd_now.artist = mpd_now.artist:upper():gsub("&.-;", string.lower)
            mpd_now.title = mpd_now.title:upper():gsub("&.-;", string.lower)
            widget:set_markup(markup.font("Roboto 4", " ")
                              .. markup.font(theme.taglist_font,
                              " " .. mpd_now.artist
                              .. " - " ..
                              mpd_now.title .. "  ") .. markup.font("Roboto 5", " "))
            play_pause_icon:set_image(theme.pause)
        elseif mpd_now.state == "pause" then
            widget:set_markup(markup.font("Roboto 4", " ") ..
                              markup.font(theme.taglist_font, " MPD PAUSED  ") ..
                              markup.font("Roboto 5", " "))
            play_pause_icon:set_image(theme.play)
        else
            widget:set_markup("")
            play_pause_icon:set_image(theme.play)
        end
    end
})
local musicbg = wibox.container.background(theme.mpd.widget, theme.bg_focus, gears.shape.rectangle)
local musicwidget = wibox.container.margin(musicbg, dpi(0), dpi(0), dpi(5), dpi(5))



musicwidget:buttons(my_table.join(awful.button({ }, 1,
function () end)))
prev_icon:buttons(my_table.join(awful.button({}, 1,
function ()
    os.execute("playerctl")
    theme.mpd.update()
end)))
next_icon:buttons(my_table.join(awful.button({}, 1,
function ()
    os.execute("mpc next")
    theme.mpd.update()
end)))
stop_icon:buttons(my_table.join(awful.button({}, 1,
function ()
    play_pause_icon:set_image(theme.play)
    os.execute("mpc stop")
    theme.mpd.update()
end)))
play_pause_icon:buttons(my_table.join(awful.button({}, 1,
function ()
    os.execute("mpc toggle")
    theme.mpd.update()
end)))
]]--

return theme
