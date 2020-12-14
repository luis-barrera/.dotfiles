--[[
    Configuración de AwesomeWM de github.com/luis-barrera
    Inspirado por:
    Awesome WM configuration template en github.com/lcpz
--]]

----------------------------------------------------------------------------------
--------------------------- Librerias necesarias ---------------------------------
----------------------------------------------------------------------------------
local awesome, client, mouse, screen, tag = awesome, client, mouse, screen, tag
local ipairs, string, os, table, tostring, tonumber, type = ipairs, string, os, table, tostring, tonumber, type
local gears         = require("gears")
local awful         = require("awful")
                      require("awful.autofocus")
local wibox         = require("wibox")
local beautiful     = require("beautiful")
local naughty       = require("naughty")
local lain          = require("lain")
local menubar       = require("menubar")
local freedesktop   = require("freedesktop")
local hotkeys_popup = require("awful.hotkeys_popup").widget
                      require("awful.hotkeys_popup.keys")
local my_table      = awful.util.table or gears.table -- 4.{0,1} compatibility
local dpi           = require("beautiful.xresources").apply_dpi

----------------------------------------------------------------------------------
------------------------ Administrador de Errorres -------------------------------
----------------------------------------------------------------------------------
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end
-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = tostring(err) })
        in_error = false
    end)
end


----------------------------------------------------------------------------------
------- Autostart para demons y aplicaciones que corren sin GUI ------------------
----------------------------------------------------------------------------------
-- Esta función se ejecuta una sola vez cada que se inicia AwesomeWM, aquí se ponen
--  programas que corren en segundo plano como dropbox o pulseaudio
local function run_once(cmd_arr)
    for _, cmd in ipairs(cmd_arr) do
        awful.spawn.with_shell(string.format("pgrep -u $USER -fx '%s' > /dev/null || (%s)", cmd, cmd))
    end
end

run_once({
          "flameshot",
          "pcloud",
          "unclutter -root", 
          -- "light-locker", --deamon del display manager, necesario para suspender el equipo
          "picom", -- deamon del compositor, permite transparencia en algunas ventanas
          -- "seahorse",
          "parcellite",
          "syncthing-gtk"
        })

-- awful.spawn("pcloud")
-- Si se usa dropbox, para que se vean mejor los iconos descargar dropbox-kde-systray-icons hardcode-tray
-- This function implements the XDG autostart specification
--[[
awful.spawn.with_shell(
    'if (xrdb -query | grep -q "^awesome\\.started:\\s*true$"); then exit; fi;' ..
    'xrdb -merge <<< "awesome.started:true";' ..
    -- list each of your autostart commands, followed by ; inside single quotes, followed by ..
    'dex --environment Awesome --autostart --search-paths "$XDG_CONFIG_DIRS/autostart:$XDG_CONFIG_HOME/autostart"' -- https://github.com/jceb/dex
)
--]]

----------------------------------------------------------------------------------
--------------------------- Definición de Variables ------------------------------
----------------------------------------------------------------------------------
local chosen_theme = "/home/luisbarrera/.config/awesome/theme.lua" -- tema
local modkey       = "Mod4" -- tecla principal, tecla Windows
local altkey       = "Mod1" -- tecla secundaria, tecla Alt izquierdo
local terminal     = "kitty" -- terminal por defect -- terminal por defectoo
local vi_focus     = false -- el foco de la ventana sigue al ratón
local cycle_prev   = false -- cycle trough all previous client or just the first
local editor       = "nvim"
local gui_editor   = "code" -- Editor en gestor grafico
local browser      = "firefox"
local scrlocker    = "light-locker"

awful.util.terminal = terminal 

awful.util.tagnames = {"home", "web", "terminal", "music", "1", "2", "3", "4"} -- nombre de los espacios
awful.util.tagnames_sec = {"a:s2", "s:s2", "d:s2"}
awful.layout.layouts = {
    lain.layout.cascade,
    awful.layout.suit.tile,
    awful.layout.suit.max,
    awful.layout.suit.magnifier,
    awful.layout.suit.floating,
    awful.layout.suit.max.fullscreen,
    lain.layout.cascade.tile,
    lain.layout.centerwork,
    --awful.layout.suit.tile.left,
    --awful.layout.suit.tile.bottom,
    --awful.layout.suit.tile.top,
    --awful.layout.suit.fair,
    --awful.layout.suit.fair.horizontal,
    --awful.layout.suit.spiral,
    --awful.layout.suit.spiral.dwindle,
    --awful.layout.suit.corner.nw,
    --awful.layout.suit.corner.ne,
    --awful.layout.suit.corner.sw,
    --awful.layout.suit.corner.se,
    --lain.layout.centerwork.horizontal,
    --lain.layout.termfair,
    --lain.layout.termfair.center,
}

-- Control de la barra(wibox) y algunos eventos de ratón relacionados a los espacios
awful.util.taglist_buttons = my_table.join(
    -- Mover al espacio al que se hace click izquierdo
    awful.button({ }, 1, function(t) t:view_only() end),
    -- Mover una ventana manteniendo tecla win y haciendo click a el espacio que queremos mover
    awful.button({ modkey }, 1, function(t)
        if client.focus then
            client.focus:move_to_tag(t)
        end
    end)
    --[[
    awful.button({ }, 3, awful.tag.viewtoggle),
    awful.button({ modkey }, 3, function(t)
        if client.focus then
            client.focus:toggle_tag(t)
        end
    end),
    ]]--
    --[[
    -- Moverse entre espacios con la rueda del ratón, 
    awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
    awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
    ]]--
)

-- Configuración del tasklist
awful.util.tasklist_buttons = my_table.join(
    -- Minimiza la tarea con click izquierdo
    awful.button({ }, 1, function (c)
        if c == client.focus then
            c.minimized = true
        else
            --c:emit_signal("request::activate", "tasklist", {raise = true})<Paste>
            -- Without this, the following
            -- :isvisible() makes no sense
            c.minimized = false
            if not c:isvisible() and c.first_tag then
                c.first_tag:view_only()
            end
            -- This will also un-minimize
            -- the client, if needed
            client.focus = c
            c:raise()
        end
    end),
    -- Cierra el cliente con click derecho
    awful.button({ }, 2, function (c) c:kill() end)
    
    --[[
    awful.button({ }, 3, function ()
        local instance = nil

        return function ()
            if instance and instance.wibox.visible then
                instance:hide()
                instance = nil
            else
                instance = awful.menu.clients({theme = {width = dpi(250)}})
            end
        end
    end)
    ]]--
    -- awful.button({ }, 4, function () awful.client.focus.byidx(1) end),
    -- awful.button({ }, 5, function () awful.client.focus.byidx(-1) end)
)

-- Aplica el tema elegido
beautiful.init(chosen_theme)

----------------------------------------------------------------------------------
--------------------------------- Menu -------------------------------------------
----------------------------------------------------------------------------------
local myawesomemenu = {
    { "hotkeys", function() return false, hotkeys_popup.show_help end },
    { "manual", terminal .. " -e man awesome" },
    { "edit config", string.format("%s -e %s %s", terminal, editor, awesome.conffile) },
    { "restart", awesome.restart },
    { "quit", function() awesome.quit() end }
}
awful.util.mymainmenu = freedesktop.menu.build({
    icon_size = beautiful.menu_height or dpi(18),
    before = {
        { "Awesome", myawesomemenu, beautiful.awesome_icon },
        -- other triads can be put here
    },
    after = {
        { "Open terminal", terminal },
        -- other triads can be put here
    }
})
-- Esconde el menu cuando se quita el mouse
awful.util.mymainmenu.wibox:connect_signal("mouse::leave", function() awful.util.mymainmenu:hide() end)
-- Establece la terminal para las apps que lo necesitan
menubar.utils.terminal = terminal 

----------------------------------------------------------------------------------
----------------------------- Pantallas ------------------------------------------
----------------------------------------------------------------------------------
-- Vuelve a poner el fondo de pantalla en caso de que la geometría de la pantalla cambie
screen.connect_signal("property::geometry", function(s)
    if beautiful.wallpaper then
        local wallpaper = beautiful.wallpaper
        -- Si wallpaper es una función
        if type(wallpaper) == "function" then
            wallpaper = wallpaper(s)
        end
        gears.wallpaper.maximized(wallpaper, s, "#ffffff", 1)
    end
end)

-- No borde cuando se organiza solo 1 ventana no flotante o maximizado
screen.connect_signal("arrange", function (s)
    local only_one = #s.tiled_clients == 1
    for _, c in pairs(s.clients) do
        if only_one and not c.floating or c.maximized then
            c.border_width = 0
        else
            c.border_width = beautiful.border_width
        end
    end
end)

function screenlayout()
  if screen:count() == 1 then
    os.execute("sh ~/.config/awesome/onescreenlayout.sh")
  else
    os.execute("sh ~/.config/awesome/screenlayout.sh")
  end
end

-- Crea una barra(wibox) para cada pantalla conectada y lo agrega
awful.screen.connect_for_each_screen(function(s) 
  beautiful.at_screen_connect(s)
  screenlayout()
end)

awful.screen.disconnect_for_each_screen(function() 
  beautiful.at_screen_connect(s)
  --screenlayout()
end)

----------------------------------------------------------------------------------
------------------------ Acciones con el mouse -----------------------------------
----------------------------------------------------------------------------------
root.buttons(my_table.join(
    -- awful.button({ }, 3, function () awful.util.mymainmenu:toggle() end)
    -- awful.button({ }, 4, awful.tag.viewnext),
    -- awful.button({ }, 5, awful.tag.viewprev)
))

----------------------------------------------------------------------------------
------------------------ Acciones con teclado ------------------------------------
----------------------------------------------------------------------------------
globalkeys = my_table.join(
    -- Shortcut para reparar problema de pantalla negra después de hacer login
    awful.key({ modkey,            }, "i", function() os.execute("xrandr --auto") end,
              {description = "Repara error despues de hacer login", group = "hotkeys"}),
    -- Screenshot
    awful.key({ modkey,           }, "Print", function() os.execute("flameshot full -c -p ~/Imágenes/screenshots") end,
              {description = "Screenshot", group = "hotkeys"}),
    -- awful.key({ }, "Print", function() os.execute("flameshot gui -p ~/Imágenes/screenshots") end,
    --        {description = "Recorte de pantalla", group = "hotkeys"}),

    -- Bloquear pantalla
--    awful.key({ modkey, "Control" }, "l", function () os.execute("light-locker-command -l") end,
--              {description = "Bloquear pantalla", group = "hotkeys"}),
    -- Apagar equipo
    awful.key({ modkey, "Control" }, "p", function () os.execute("poweroff") end,
              {description = "Apagar equipo", group = "hotkeys"}),

    -- Muestra la configuración del teclado
    awful.key({ modkey,           }, "z", hotkeys_popup.show_help,
              {description = "Muestra esta pantalla", group="awesome"}),

    -- Navegación entre espacios
    awful.key({ modkey,           }, "Left", awful.tag.viewprev,
              {description = "Espacio previo", group = "tag"}),
    awful.key({ modkey,           }, "Right", awful.tag.viewnext,
              {description = "Espacio siguiente", group = "tag"}),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
              {description = "Espacio anterior", group = "tag"}),

    -- Navegación entre espacios no vacios
    awful.key({ altkey,           }, "Left", function () lain.util.tag_view_nonempty(-1) end,
              {description = "Espacio no vacio previo", group = "tag"}),
    awful.key({ altkey,           }, "Right", function () lain.util.tag_view_nonempty(1) end,
              {description = "Espacio no vacio siguiente", group = "tag"}),

    -- Navegación entre clientes por indice
    awful.key({ altkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
        end,
        {description = "Siguiente por indice", group = "client"}
    ),
    awful.key({ altkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
        end,
        {description = "Anterior por indice", group = "client"}
    ),

    -- Navegación entre clientes por dirección
    awful.key({ modkey,           }, "j",
        function()
            awful.client.focus.global_bydirection("down")
            if client.focus then client.focus:raise() end
        end,
        {description = "Focus abajo", group = "client"}),
    awful.key({ modkey,           }, "k",
        function()
            awful.client.focus.global_bydirection("up")
            if client.focus then client.focus:raise() end
        end,
        {description = "Focus arriba", group = "client"}),
    awful.key({ modkey,           }, "h",
        function()
            awful.client.focus.global_bydirection("left")
            if client.focus then client.focus:raise() end
        end,
        {description = "Focus izquierdo", group = "client"}),
    awful.key({ modkey,           }, "l",
        function()
            awful.client.focus.global_bydirection("right")
            if client.focus then client.focus:raise() end
        end,
        {description = "Focus derecha", group = "client"}),

    -- Manipulación de la disposición de los clientes
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1) end,
              {description = "Mover cliente a la derecha", group = "client"}),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1) end,
              {description = "Mover cliente a la izquierda", group = "client"}),

    -- Focus a la siguiente pantalla
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end,
              {description = "Focus siguiente pantalla", group = "screen"}),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end,
              {description = "Focus pantalla anterior", group = "screen"}),

    -- Mueve al cliente en estado urgente
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
              {description = "Mover a cliente urgente", group = "client"}),

    -- Cicla entre los clientes del espacio
    awful.key({ modkey,           }, "Tab",
        function ()
            if cycle_prev then
                awful.client.focus.history.previous()
            else
                awful.client.focus.byidx(-1)
            end
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "Cicla entre los clientes", group = "client"}),

     -- Muestra o esconde la barra 
    awful.key({ modkey,           }, "b", function ()
            for s in screen do
              if s.index == 1 then
                s.mywibox.visible = not s.mywibox.visible
              end
            end
        end,
        {description = "Muestra/Oculta Wibox Superior", group = "awesome"}),

   -- Muestra o esconde la barra 
    awful.key({ modkey, "Shift"   }, "b", function ()
        local status = not screen[1].mybottomwibox.visible
        -- TODO: obtener el valor de la visibilidad del wibox inferior de la primera pantalla y basarse en el para establecer la visibilidad de los demás
            for s in screen do
                if s.mywibox then
                  -- s.mywibox.visible = not s.mywibox.visible
                  s.mywibox.visible = status
                end
                if s.mybottomwibox then
                    -- s.mybottomwibox.visible = not s.mybottomwibox.visible
                  s.mybottomwibox.visible = status
                end
            end
        end,
        {description = "Muestra/Oculta Wibox", group = "awesome"}),

    -- Tamaño del padding de los clientes (gaps)
    awful.key({ altkey, "Control" }, "+", function () lain.util.useless_gaps_resize(1) end,
              {description = "Aumenta padding", group = "tag"}),
    awful.key({ altkey, "Control" }, "-", function () lain.util.useless_gaps_resize(-1) end,
              {description = "Decrementa padding", group = "tag"}),

    -- Espacios Dinámicos
    awful.key({ modkey, "Shift"   }, "n", function () lain.util.add_tag() end,
              {description = "Nuevo espacio", group = "tag"}),
    awful.key({ modkey, "Shift"   }, "r", function () lain.util.rename_tag() end,
              {description = "Cambiar nombre", group = "tag"}),
    awful.key({ modkey, "Shift"   }, "Left", function () lain.util.move_tag(-1) end,
              {description = "Mover espacio a la izquierda", group = "tag"}),
    awful.key({ modkey, "Shift"   }, "Right", function () lain.util.move_tag(1) end,
              {description = "Mover espacio a la derecha", group = "tag"}),
    awful.key({ altkey, "Shift"   }, "d", function () lain.util.delete_tag() end,
              {description = "Eliminar espacio", group = "tag"}),

    -- Ajustes de Awesome
    awful.key({ modkey, "Control" }, "r", awesome.restart,
              {description = "Recargar Awesome", group = "awesome"}),
    awful.key({ modkey, "Control" }, "o", awesome.quit,
              {description = "Cerrar Sesión", group = "awesome"}),

    -- Modificar tamaño del cliente
    awful.key({ altkey, "Shift"   }, "l", function () awful.tag.incmwfact( 0.05) end,
              {description = "Aumenta el tamaño", group = "layout"}),
    awful.key({ altkey, "Shift"   }, "h", function () awful.tag.incmwfact(-0.05) end,
              {description = "Disminuye el tamaño", group = "layout"}),

    -- Número de cliente maestros
    awful.key({ modkey, "Shift"   }, "h", function () awful.tag.incnmaster( 1, nil, true) end,
              {description = "Aumenta clientes maestros", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "l", function () awful.tag.incnmaster(-1, nil, true) end,
              {description = "Decrementa clientes maestros", group = "layout"}),

    -- Número de columnas
    awful.key({ modkey, "Control" }, "h", function () awful.tag.incncol( 1, nil, true) end,
              {description = "Incrementa numero de columnas", group = "layout"}),
    awful.key({ modkey, "Control" }, "l", function () awful.tag.incncol(-1, nil, true) end,
              {description = "Decrementa numero de columnas", group = "layout"}),

    -- Seleccionar el layout 
    awful.key({ modkey,           }, "space", function () awful.layout.inc( 1) end,
              {description = "Siguiente Layout", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1) end,
              {description = "Anterior Layout", group = "layout"}),

    -- Muestra las ventanas minimizadas
    awful.key({ modkey, "Control" }, "n",
              function ()
                  local c = awful.client.restore()
                  -- Focus restored client
                  if c then
                      client.focus = c
                      c:raise()
                  end
              end,
              {description = "Mostrar minimizado", group = "client"}),

    -- Brillo de pantalla
    awful.key({ }, "XF86MonBrightnessUp", function () os.execute("xbacklight -inc 5") end,
              {description = "Subir Brillo 5%", group = "hotkeys"}),
    awful.key({ }, "XF86MonBrightnessDown", function () os.execute("xbacklight -dec 10") end,
              {description = "Bajar Brillo 10%", group = "hotkeys"}),

    -- Control de volumen y música
    awful.key({ modkey, "Control" }, "Left",
         function ()
            os.execute("playerctl --player=playerctld previous")
            -- beautiful.volume.update()- agregar una función que actualice la canción actual
        end,
        {description = "Cancion Anterior", group = "media"}),
    awful.key({ modkey, "Control" }, "Right",
         function ()
            os.execute("playerctl --player=playerctld next")
            -- beautiful.volume.update()- agregar una función que actualice la canción actual
        end,
        {description = "Cancion Siguiente", group = "media"}),
    awful.key({ modkey, "Control" }, "space",
         function ()
            os.execute("playerctl --player=playerctld play-pause")
            -- beautiful.volume.update()- agregar una función que actualice la canción actual
        end,
        {description = "Play - Pause", group = "media"}),
    awful.key({ modkey, "Control" }, "Up",
        function ()
            os.execute(string.format("amixer -q set %s 1%%+", beautiful.volume.channel))
            beautiful.volume.update()
        end,
        {description = "Subir volumen", group = "media"}),
    awful.key({ modkey, "Control" }, "Down",
        function ()
            os.execute(string.format("amixer -q set %s 1%%-", beautiful.volume.channel))
            beautiful.volume.update()
        end,
        {description = "Bajar Volumen", group = "media"}),
    awful.key({ }, "XF86AudioRaiseVolume",
        function ()
            os.execute(string.format("amixer -q set %s 1%%+", beautiful.volume.channel))
            beautiful.volume.update()
        end,
        {description = "Subir volumen", group = "media"}),
    awful.key({ }, "XF86AudioLowerVolume",
        function ()
            os.execute(string.format("amixer -q set %s 1%%-", beautiful.volume.channel))
            beautiful.volume.update()
        end,
        {description = "Bajar Volumen", group = "media"}),
    awful.key({ }, "XF86AudioMute",
        function ()
            os.execute(string.format("amixer -q set %s toggle", beautiful.volume.togglechannel or beautiful.volume.channel))
            beautiful.volume.update()
        end,
        {description = "Mute sonido", group = "media"}),
    awful.key({ }, "XF86AudioMicMute",
        function ()
            os.execute(string.format("amixer -q set Capture toggle", beautiful.volume.togglechannel or beautiful.volume.channel))
            beautiful.volume.update()
        end,
        {description = "Mute sonido", group = "media"}),


    -- Lanzar algunos programas
    awful.key({ modkey, "Shift"   }, "q", function () awful.spawn(browser) end,
              {description = "Abrir Firefox", group = "launcher"}),
    awful.key({ modkey, "Shift"   }, "w", function () awful.spawn("nemo") end,
              {description = "Abrir nemo", group = "launcher"}),
    awful.key({ modkey,           }, "Return", function () awful.spawn(terminal) end,
              {description = "Abrir terminal", group = "launcher"}),
    awful.key({ modkey, "Shift"   }, "z", function () awful.spawn("rofi -show keys -theme dmenu") end,
              {description = "Mostrar keys extras", group = "launcher"}),
    awful.key({ modkey,           }, "x", function () awful.spawn("rofi -show combi -theme gruvbox-dark") end,
              {description = "dmenu", group = "launcher"})

    --[[
    awful.key({ altkey, "Control" }, "m",
        function ()
            os.execute(string.format("amixer -q set %s 100%%", beautiful.volume.channel))
            beautiful.volume.update()
        end,
        {description = "volume 100%", group = "hotkeys"}),
    awful.key({ altkey, "Control" }, "0",
        function ()
            os.execute(string.format("amixer -q set %s 0%%", beautiful.volume.channel))
            beautiful.volume.update()
        end,
        {description = "volume 0%", group = "hotkeys"}),
    ]]--

    --[[
    -- MPD control
    awful.key({ altkey, "Control" }, "Up",
        function ()
            os.execute("mpc toggle")
            beautiful.mpd.update()
        end,
        {description = "mpc toggle", group = "widgets"}),
    awful.key({ altkey, "Control" }, "Down",
        function ()
            os.execute("mpc stop")
            beautiful.mpd.update()
        end,
        {description = "mpc stop", group = "widgets"}),
    awful.key({ altkey, "Control" }, "Left",
        function ()
            os.execute("mpc prev")
            beautiful.mpd.update()
        end,
        {description = "mpc prev", group = "widgets"}),
    awful.key({ altkey, "Control" }, "Right",
        function ()
            os.execute("mpc next")
            beautiful.mpd.update()
        end,
        {description = "mpc next", group = "widgets"}),
    awful.key({ altkey }, "0",
        function ()
            local common = { text = "MPD widget ", position = "top_middle", timeout = 2 }
            if beautiful.mpd.timer.started then
                beautiful.mpd.timer:stop()
                common.text = common.text .. lain.util.markup.bold("OFF")
            else
                beautiful.mpd.timer:start()
                common.text = common.text .. lain.util.markup.bold("ON")
            end
            naughty.notify(common)
        end,
        {description = "mpc on/off", group = "widgets"}),
    ]]--

    --[[
    -- Copy primary to clipboard (terminals to gtk)
    awful.key({ modkey }, "c", function () awful.spawn.with_shell("xsel | xsel -i -b") end,
              {description = "copy terminal to gtk", group = "hotkeys"}),
    -- Copy clipboard to primary (gtk to terminals)
    awful.key({ modkey }, "v", function () awful.spawn.with_shell("xsel -b | xsel") end,
              {description = "copy gtk to terminal", group = "hotkeys"}),
    ]]--

    --[[ 
    -- Menubar
    awful.key({ modkey }, "p", function() menubar.show() end,
              {description = "show the menubar", group = "launcher"})
    --]]
    --[[ dmenu
    awful.key({ modkey }, "x", function ()
            os.execute(string.format("dmenu_run -i -fn 'Monospace' -nb '%s' -nf '%s' -sb '%s' -sf '%s'",
            beautiful.bg_normal, beautiful.fg_normal, beautiful.bg_focus, beautiful.fg_focus))
        end,
        {description = "show dmenu", group = "launcher"})
    --]]
    -- alternatively use rofi, a dmenu-like application with more features
    -- check https://github.com/DaveDavenport/rofi for more details
    --[[ rofi
    awful.key({ modkey }, "x", function ()
            os.execute(string.format("rofi -show %s -theme %s",
            'run', 'dmenu'))
        end,
        {description = "show rofi", group = "launcher"}),
    --]]
    
    -- Mostrar menú
    --awful.key({ modkey,           }, "w", function () awful.util.mymainmenu:show() end,
    --          {description = "show main menu", group = "awesome"}),

    -- Cl
    --[[awful.key({ modkey, "Shift"   }, "Tab",
        function ()
            if cycle_prev then
                awful.client.focus.byidx(1)
                if client.focus then
                    client.focus:raise()
                end
            end
        end,
        {description = "go forth", group = "client"}),
    ]]--

    --[[
    -- Dropdown application -- no lo utilizo ni sé para qué sirve 
    awful.key({ modkey, }, "z", function () awful.screen.focused().quake:toggle() end,
              {description = "dropdown application", group = "launcher"}),

    -- Widgets popups
    awful.key({ altkey, }, "c", function () if beautiful.cal then beautiful.cal.show(4) end end,
              {description = "show calendar", group = "widgets"}),
    awful.key({ altkey, }, "h", function () if beautiful.fs then beautiful.fs.show(7) end end,
              {description = "show filesystem", group = "widgets"}),
    awful.key({ altkey, }, "w", function () if beautiful.weather then beautiful.weather.show(7) end end,
              {description = "show weather", group = "widgets"}),
    ]]--

)

clientkeys = my_table.join(
    awful.key({ modkey, "Shift"   }, "m",  lain.util.magnify_client,
              {description = "Maximizar cliente", group = "client"}),
    awful.key({ modkey,           }, "e", function (c) c:kill() end,
              {description = "Cerrar cliente", group = "client"}),
    awful.key({ altkey,           }, "space", awful.client.floating.toggle,
              {description = "Floating", group = "client"}),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
              {description = "Mover a master", group = "client"}),
    awful.key({ modkey,           }, "o", function (c) c:move_to_screen() end,
              {description = "Mover a pantalla", group = "client"}),
    awful.key({ modkey,           }, "t", function (c) c.ontop = not c.ontop end,
              {description = "Mantener al frente", group = "client"}),
    awful.key({ modkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end ,
        {description = "Minimizar", group = "client"}),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized = not c.maximized
            c:raise()
        end ,
        {description = "Maximizar", group = "client"})
)

-- Asigna espacios a teclas de numeros
for i = 1, 9 do
    -- Hack to only show tags 1 and 9 in the shortcut window (mod+s)
    local descr_view, descr_toggle, descr_move, descr_toggle_focus
    if i == 1 or i == 9 then
        descr_view = {description = "Ver #", group = "tag"}
        descr_toggle = {description = "Cambiar #", group = "tag"}
        descr_move = {description = "Mover cliente a #", group = "tag"}
        descr_toggle_focus = {description = "Cambiar focus a #", group = "tag"}
    end
    local j = i + 4
    globalkeys = my_table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = awful.screen.focused()
                        local tag = screen.tags[j]
                        if tag then
                           tag:view_only()
                        end
                  end,
                  descr_view),
        -- Toggle tag display.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = awful.screen.focused()
                      local tag = screen.tags[j]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end,
                  descr_toggle),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[j]
                          if tag then
                              client.focus:move_to_tag(tag)
                          end
                     end
                  end,
                  descr_move),
        -- Toggle tag on focused client.
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[j]
                          if tag then
                              client.focus:toggle_tag(tag)
                          end
                      end
                  end,
                  descr_toggle_focus)
    )
end

-- Asigna espacios a las letras
local t = {'a', 's', 'd', 'f'}
for i = 1, 4 do
    local j = t[i]
    globalkeys = my_table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, j,
                  function ()
                        local screen = awful.screen.focused()
                        local tag = screen.tags[i]
                        if tag then
                           tag:view_only()
                        end
                  end,
                  descr_view),
        -- Toggle tag display.
        awful.key({ modkey, "Control" }, j,
                  function ()
                      local screen = awful.screen.focused()
                      local tag = screen.tags[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end,
                  descr_toggle),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, j,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:move_to_tag(tag)
                          end
                     end
                  end,
                  descr_move),
        -- Toggle tag on focused client.
        awful.key({ modkey, "Control", "Shift" }, j,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:toggle_tag(tag)
                          end
                      end
                  end,
                  descr_toggle_focus)
    ) 
end

clientbuttons = gears.table.join(
    awful.button({ }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
    end),
    -- Cambiar posición del cliente
    awful.button({ modkey }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.move(c)
    end),
    -- Cambiar tamaño del cliente
    awful.button({ modkey }, 3, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.resize(c)
    end)
)

-- Establecer las keybindings 
root.keys(globalkeys)

----------------------------------------------------------------------------------
------------------------------ Reglas de ventanas --------------------------------
----------------------------------------------------------------------------------
-- Reglas que deben seguir los clientes (ventanas)(a traves de la señal "manage").
awful.rules.rules = {
    -- Las reglas para todos los clientes
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     screen = awful.screen.preferred,
                     placement = awful.placement.no_overlap+awful.placement.no_offscreen,
                     size_hints_honor = false,
                     -- maximized = true,
     }
    },

    -- Barra de título
    { rule_any = { type = { "dialog", "normal" } },
      properties = { titlebars_enabled = false } },

    -- Configuración para clientes de Firefox.
    { rule = { class = "firefox" },
      properties = { screen = 1, tag = awful.util.tagnames[2], titlebars_enabled = false } },

    -- Spotify
    { rule = { class = "spotify" },  
      properties = { tag = "music", titlebars_enabled = false } },

    -- KeePassXC
    { rule = { class = "KeePassXC" },  
      properties = { floating = true } },

    -- zoom
    { rule = { class = "zoom" },  
      properties = { floating = true, titlebars_enabled = false } },

    -- pomodoro 
    { rule = { class = "Solanum" },  
      properties = { floating = true, titlebars_enabled = false } },


    { rule = { class = "Gimp", role = "gimp-image-window" },
      properties = { maximized = true } },
}

----------------------------------------------------------------------------------
----------------- Acciones específicas para nuevos clientes ----------------------
----------------------------------------------------------------------------------
-- Funciones que se ejecutan una vez que aperece un nuevo cliente.
client.connect_signal("manage", function (c)
    -- if not awesome.startup then awful.client.setslave(c) end
    -- Establece la ventana como hijo
    if awesome.startup and
      not c.size_hints.user_position
      and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end
end)

-- Formato de la titlebar en caso de que esté activada en las reglas 
client.connect_signal("request::titlebars", function(c)
    -- Personalizado
    if beautiful.titlebar_fun then
        beautiful.titlebar_fun(c)
        return
    end

    -- Default
    -- Acciones con el ratón
    local buttons = my_table.join(
        -- Mover cliente con mouse
        awful.button({ }, 1, function()
            c:emit_signal("request::activate", "titlebar", {raise = true})
            awful.mouse.client.move(c)
        end),
        -- Matar cliente con click central
        awful.button({ }, 2, function() c:kill() end),
        -- Cambiar tamaño arrastrando el mouse
        awful.button({ }, 3, function()
            c:emit_signal("request::activate", "titlebar", {raise = true})
            awful.mouse.client.resize(c)
        end)
    )

    -- Disposición de los elementos
    awful.titlebar(c, {size = dpi(16)}) : setup {
        { -- Izquierda
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Medio
            { -- Nombre 
                align  = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = wibox.layout.flex.horizontal
        },
        { -- Derecha
            awful.titlebar.widget.floatingbutton (c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.stickybutton   (c),
            awful.titlebar.widget.ontopbutton    (c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)

-- Activa el sloppy focus, para que el focus siga el mouse
client.connect_signal("mouse::enter", function(c)
    c:emit_signal("request::activate", "mouse_enter", {raise = vi_focus})
end)

-- Cambiar el color de los bordes de los clientes
client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

-- Fin
