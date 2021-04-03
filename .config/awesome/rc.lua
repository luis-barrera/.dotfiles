--[[
    AwesomeWM configuration files: github.com/luis-barrera
    Inspired by: github.com/lcpz
--]]

-- Libraries
------------
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

-- Error Catching
-----------------
-- If an error occurs during the load of the configuration file, it display a notification
--    and then loads a basic configuration file.
if awesome.startup_errors then
  naughty.notify({ preset = naughty.config.presets.critical,
    title = "Oops, there were errors during startup!",
    text = awesome.startup_errors })
end

do
  local in_error = false
  awesome.connect_signal("debug::error", function(err)
    if in_error then return end
    in_error = true
    naughty.notify({ preset = naughty.config.presets.critical,
      title = "Oops, an error happened!",
      text = tostring(err) })
    in_error = false
  end)
end

-- Autostart
------------
-- This function is executed once, the first time you enter the WM. It's useful for demons and application
--    that runs in the background.
local function run_once(cmd_arr)
  for _, cmd in ipairs(cmd_arr) do
    awful.spawn.with_shell(string.format("pgrep -u $USER -fx '%s' > /dev/null || %s", cmd, cmd))
    -- awful.spawn.with_shell(string.format("pgrep -u $USER -fx '%s' > /dev/null || (%s)", cmd, cmd))
  end
end

run_once({
  -- "seahorse",
  -- "light-locker", -- Display manager's Daemon, necessary for suspend your laptop
  -- "lxsession" -- Polkit, used by software that needs authentication
  -- "picom -b", -- Compositor's Daemon, allows transparency in some windows (clients)
  "nm-applet", -- NetworkManager applet, usefull to easily connect to a network
  "flameshot", -- Software used to take Screenshots
  "unclutter", -- Hides the cursor when is not used
  "parcellite -d", -- Clipboard software
  "syncthing-gtk" -- Simple Local Network File Sync Software
})

-- This function implements the XDG autostart specification
--[[
awful.spawn.with_shell(
  'if (xrdb -query | grep -q "^awesome\\.started:\\s*true$"); then exit; fi;' ..
  'xrdb -merge <<< "awesome.started:true";' ..
  -- list each of your autostart commands, followed by ; inside single quotes, followed by ..
  'dex --environment Awesome --autostart --search-paths "$XDG_CONFIG_DIRS/autostart:$XDG_CONFIG_HOME/autostart"' -- https://github.com/jceb/dex
)
--]]

-- Useful Variables Definition
------------------------------
local chosen_theme = "/home/luisbarrera/.config/awesome/theme.lua" -- Theme
local modkey       = "Mod4" -- Principal Key, Windows Key
local altkey       = "Mod1" -- Secondary Key, Left Alt key
local terminal     = "kitty" -- Default Terminal Emulator
local editor       = "nvim" -- Text Editor in the Terminal
local gui_editor   = "geany" -- Default GUI editor
local browser      = "firefox" -- Default Internet Browser
local scrlocker    = "light-locker" -- Screenlocker
local vi_focus     = true -- The focus of the client (window) follows the coursor
local cycle_prev   = false -- Cycle trough all previous client or just the first

awful.util.terminal = terminal -- Defines the default terminal emulator
beautiful.init(chosen_theme) -- Applies theme

-- Tags (workspaces) names
awful.util.ws_keys = {'a', 's', 'd', 'f', 'q', 'w', 'e', '1', '2', '3', '4', '5'}
awful.util.tagnames = {"home", "web", "terminal", "music", "cal", "email", "vimwiki", "1", "2", "3", "4", "5"}
awful.util.tagnames_sec = {"a2", "s2", "d2", "f2"} -- Workspaces for extra monitors
awful.layout.layouts = { -- Clients Layouts
  lain.layout.cascade,
  awful.layout.suit.tile,
  awful.layout.suit.max,
  awful.layout.suit.magnifier,
  awful.layout.suit.floating,
  awful.layout.suit.max.fullscreen,
  lain.layout.cascade.tile,
  -- lain.layout.centerwork,
  -- awful.layout.suit.tile.left,
  -- awful.layout.suit.tile.bottom,
  -- awful.layout.suit.tile.top,
  -- awful.layout.suit.fair,
  -- awful.layout.suit.fair.horizontal,
  -- awful.layout.suit.spiral,
  -- awful.layout.suit.spiral.dwindle,
  -- awful.layout.suit.corner.nw,
  -- awful.layout.suit.corner.ne,
  -- awful.layout.suit.corner.sw,
  -- awful.layout.suit.corner.se,
  -- lain.layout.centerwork.horizontal,
  -- lain.layout.termfair,
  -- lain.layout.termfair.center,
}

-- Mouse Actions
----------------
-- Actions with the mouse in the taglist (bar with the names of the workspaces)
awful.util.taglist_buttons = my_table.join(
  -- Left click shows the workspace
  awful.button({ }, 1, function(t) t:view_only() end),
  -- Move a client to a workspace with Win Key and Left Click
  awful.button({ modkey }, 1, function(t)
    if client.focus then
      client.focus:move_to_tag(t)
    end
  end),
  -- Select several workspaces with Right Click, shows all the clients in only one workspace
  awful.button({ }, 3, awful.tag.viewtoggle),
  -- Shows the actual client in several workspaces with Right Click and WinKey
  awful.button({ modkey }, 3, function(t)
    if client.focus then
      client.focus:toggle_tag(t)
    end
  end),
  -- Scroll throug workspaces with the mouse's scroll wheel
  awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
  awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
)

-- Actions with the mouse in the tasklist (bar with the name of the open applications)
awful.util.tasklist_buttons = my_table.join(
  -- Minimize/Desminimize a client with left click
  awful.button({ }, 1, function (c)
    if c == client.focus then
      c.minimized = true
    else
      c.minimized = false
      if not c:isvisible() and c.first_tag then
        c.first_tag:view_only()
      end
      client.focus = c
      c:raise()
    end
  end),
  -- With central click, closes the client
  awful.button({ }, 2, function (c) c:kill() end),
  -- Move throug clients with the scroll wheel
  awful.button({ }, 4, function () awful.client.focus.byidx(1) end),
  awful.button({ }, 5, function () awful.client.focus.byidx(-1) end)
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
  end),
]]--
)

root.buttons(my_table.join(
  -- Abre menú de aplicaciones con click derecho en cualquier lado libre
  awful.button({ }, 3, function() awful.util.mymainmenu:toggle() end)
  -- Mueve al siguiente espacio si hacemos rueda del ratón arriba
  --awful.button({ }, 4, awful.tag.viewnext),
  -- Mueve al anterior espacio si hacemos rueda del ratón abajo
  --awful.button({ }, 5, awful.tag.viewprev)
))

-- Keyboard Actions
-------------------
globalkeys = my_table.join(
  -- Shortcut para reparar problema de pantalla negra después de hacer login
  awful.key({ modkey }, "i", function()
    awful.spawn("xrandr --auto")
  end, {description = "Repara error despues de hacer login", group = "hotkeys"}),

  -- Screenshot
  awful.key({ modkey }, "Print", function()
    awful.spawn("flameshot full -c -p /home/luisbarrera/screenshots")
  end, {description = "Screenshot", group = "hotkeys"}),
  -- Recorte de pantalla
  awful.key({ modkey, "Shift" }, "Print", function()
    awful.spawn("flameshot gui -p /home/luisbarrera/screenshots")
  end, {description = "Recorte de pantalla", group = "hotkeys"}),

  -- Navegación entre espacios
  awful.key({ modkey }, "Left", awful.tag.viewprev, {description = "Espacio previo", group = "tag"}),
  awful.key({ modkey }, "Right", awful.tag.viewnext, {description = "Espacio siguiente", group = "tag"}),
  awful.key({ modkey }, "Escape", awful.tag.history.restore, {description = "Espacio anterior", group = "tag"}),

  -- Navegación entre espacios no vacios
  awful.key({ altkey }, "Left", function()
    lain.util.tag_view_nonempty(-1)
  end, {description = "Espacio no vacío anterior", group = "tag"}),
  awful.key({ altkey }, "Right", function()
    lain.util.tag_view_nonempty(1)
  end, {description = "Espacio no vacío siguiente", group = "tag"}),

  -- Navegación entre clientes por indice
  awful.key({ altkey }, "j", function()
    awful.client.focus.byidx(1)
  end, {description = "Siguiente por indice", group = "client"}),
  awful.key({ altkey }, "k", function()
    awful.client.focus.byidx(-1)
  end, {description = "Anterior por indice", group = "client"}),

  -- Navegación entre clientes por dirección
  awful.key({ modkey }, "j", function()
    awful.client.focus.global_bydirection("down")
    if client.focus then
        client.focus:raise()
    end
  end, { description = "Focus abajo", group = "client" }),
  awful.key({ modkey }, "k", function()
    awful.client.focus.global_bydirection("up")
    if client.focus then
      client.focus:raise()
    end
  end, { description = "Focus arriba", group = "client" }),
  awful.key({ modkey }, "h", function()
    awful.client.focus.global_bydirection("left")
    if client.focus then
      client.focus:raise()
    end
  end, { description = "Focus izquierdo", group = "client" }),
  awful.key({ modkey }, "l", function()
    awful.client.focus.global_bydirection("right")
    if client.focus then
      client.focus:raise()
    end
  end, { description = "Focus derecha", group = "client" }),

  -- Manipulación de la disposición de los clientes
  awful.key({ modkey, "Shift" }, "j", function()
    awful.client.swap.byidx(1)
  end, {description = "Mover cliente a la derecha", group = "client"}),
  awful.key({ modkey, "Shift" }, "k", function()
    awful.client.swap.byidx(-1)
  end, {description = "Mover cliente a la izquierda", group = "client"}),

  -- Número de clientes en la columna (da la sensación de mover clientes horizontalmente)
  awful.key({ modkey, "Shift" }, "h", function()
    awful.tag.incnmaster(1, nil, true)
  end, {description = "Mueve cliente a la izquierda", group = "layout"}),
  awful.key({ modkey, "Shift" }, "l", function()
    awful.tag.incnmaster(-1, nil, true)
  end, {description = "Mueve cliente a la derecha", group = "layout"}),

  -- Focus a la siguiente pantalla
  awful.key({ modkey, "Control" }, "h", function()
    awful.screen.focus_relative(1)
  end, {description = "Focus siguiente pantalla", group = "screen"}),
  awful.key({ modkey, "Control" }, "l", function()
    awful.screen.focus_relative(-1)
  end, {description = "Focus pantalla anterior", group = "screen"}),

  -- Cicla entre los clientes del espacio
  awful.key({ modkey }, "Tab", function()
    if cycle_prev then
      awful.client.focus.history.previous()
    else
      awful.client.focus.byidx(-1)
    end
    if client.focus then
      client.focus:raise()
    end
  end, {description = "Cicla entre los clientes", group = "client"}),

  -- Muestra o esconde la barra
  awful.key({ modkey }, "y", function()
    local s = awful.screen.focused({mouse = true})
    if s.mywibox then
      s.mywibox.visible = not s.mywibox.visible
    end
  end, {description = "Muestra/Oculta Wibox Superior", group = "awesome"}),

  -- Muestra o esconde las barras superior e inferior basandose en el estado de la barra inferior
  awful.key({ modkey, "Shift" }, "y", function()
    local s = awful.screen.focused({mouse = true})
    local status = not s.mybottomwibox.visible

    if s.mywibox then
      s.mywibox.visible = status
    end
    if s.mybottomwibox then
      s.mybottomwibox.visible = status
    end
  end, {description = "Muestra/Oculta Wibox", group = "awesome"}),

  -- Tamaño del padding de los clientes (gaps)
  awful.key({ altkey, "Control" }, "+", function()
    lain.util.useless_gaps_resize(1)
  end, {description = "Aumenta padding", group = "tag"}),
  awful.key({ altkey, "Control" }, "-", function()
    lain.util.useless_gaps_resize(-1)
  end, {description = "Decrementa padding", group = "tag"}),

  -- Número de columnas
  awful.key({ modkey, "Control" }, "h", function()
    awful.tag.incncol(1, nil, true)
  end, {description = "Incrementa numero de columnas", group = "layout"}),
  awful.key({ modkey, "Control" }, "l", function()
    awful.tag.incncol(-1, nil, true)
  end, {description = "Decrementa numero de columnas", group = "layout"}),

  -- Seleccionar el layout
  awful.key({ modkey }, "space", function()
    awful.layout.inc(1)
  end, {description = "Siguiente Layout", group = "layout"}),
  awful.key({ modkey, "Shift"   }, "space", function()
    awful.layout.inc(-1)
  end, {description = "Anterior Layout", group = "layout"}),

  -- Muestra las ventanas minimizadas
  awful.key({ modkey, "Control" }, "n", function()
    local c = awful.client.restore()

    if c then
      client.focus = c
      c:raise()
    end
  end, {description = "Mostrar minimizado", group = "client"}),

  -- Brillo de pantalla
  awful.key({ }, "XF86MonBrightnessUp", function()
    awful.spawn("xbacklight -inc 5")
  end, {description = "Subir Brillo 5%", group = "hotkeys"}),
  awful.key({ }, "XF86MonBrightnessDown", function()
    awful.spawn("xbacklight -dec 10")
  end, {description = "Bajar Brillo 10%", group = "hotkeys"}),
  awful.key({ altkey }, "XF86MonBrightnessDown", function()
    awful.spawn("xbacklight -set 0.06")
  end, {description = "Bajar Brillo al Mínimo", group = "hotkeys"}),

  -- Control de música
  awful.key({ modkey, "Control" }, "Left", function()
    awful.spawn("playerctl --player=playerctld previous")
  end, {description = "Cancion Anterior", group = "media"}),
  awful.key({ modkey, "Control" }, "Right", function()
    awful.spawn("playerctl --player=playerctld next")
  end, {description = "Cancion Siguiente", group = "media"}),
  awful.key({ modkey, "Control" }, "space", function()
    awful.spawn("playerctl --player=playerctld play-pause")
  end, {description = "Play - Pause", group = "media"}),

  -- Control de Volumen
  awful.key({ modkey, "Control" }, "Up", function()
    awful.spawn("pulseaudio-ctl up 2")
  end, {description = "Subir volumen", group = "media"}),
  awful.key({ modkey, "Control" }, "Down", function()
    awful.spawn("pulseaudio-ctl down 5")
  end, {description = "Bajar Volumen", group = "media"}),
  awful.key({ }, "XF86AudioRaiseVolume", function()
    awful.spawn("pulseaudio-ctl up 2")
  end, {description = "Subir volumen", group = "media"}),
  awful.key({ }, "XF86AudioLowerVolume", function()
    awful.spawn("pulseaudio-ctl down 5")
  end, {description = "Bajar Volumen", group = "media"}),
  awful.key({ }, "XF86AudioMute", function()
    awful.spawn("pulseaudio-ctl mute")
  end, {description = "Mute Sonido", group = "media"}),
  awful.key({ }, "XF86AudioMicMute", function()
    awful.spawn("pulseaudio-ctl mute-input")
  end, {description = "Mute micrófono", group = "media"}),

  -- Muestra la configuración del teclado
  awful.key({ modkey, "Shift" }, "z", hotkeys_popup.show_help, {description = "Muestra esta pantalla", group="awesome"}),

  -- Ajustes de Awesome
  awful.key({ modkey, "Control" }, "r", awesome.restart, {description = "Recargar Awesome", group = "awesome"}),
  awful.key({ modkey, "Control" }, "o", awesome.quit, {description = "Cerrar Sesión", group = "awesome"}),

  -- Abrir Firefox
  awful.key({ modkey, "Shift" }, "b", function()
    awful.spawn(browser)
  end, {description = "Abrir Firefox", group = "launcher"}),
  -- awful.key({ modkey, "Control", "Shift" }, "q", function ()
  --   awful.spawn("brave")
  -- end, {description = "Abrir Firefox", group = "launcher"}),

  -- Abrir Nemo
  awful.key({ modkey, "Shift" }, "c", function()
    awful.spawn("nemo")
  end, {description = "Abrir nemo", group = "launcher"}),

  -- Abre una terminal
  awful.key({ modkey }, "Return", function()
    awful.spawn(terminal)
  end, {description = "Abrir terminal", group = "launcher"}),

  -- Muestra keys adicionales
  -- awful.key({ modkey, "Shift" }, "x", function()
  --   awful.spawn("rofi -show keys")
  -- end, {description = "Mostrar keys extras", group = "launcher"}),

  -- Menú de apagado
  -- awful.key({ modkey }, "p", function()
  --   awful.spawn("sh $HOME/scripts/powermenu/powermenu.sh")
  -- end, {description = "Menú de Apagado", group = "hotkeys"})

  -- Menú ROFI
  -- awful.key({ modkey }, "x", function()
  --   awful.spawn("rofi -no-lazy-grab -show drun -modi drun -theme ~/scripts/launcher/launchpad.rasi")
  -- end, {description = "dmenu", group = "launcher"}),
  awful.key({ modkey }, "z", function()
    awful.spawn("rofi -show combi")
  end, {description = "dmenu", group = "launcher"})

  -- No poner nada entre esta linea y el parentesis cerrando
)

--[[ Keybindings que no uso
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
  awful.key({ altkey, "Control" }, "m", function ()
              os.execute(string.format("amixer -q set %s 100%%", beautiful.volume.channel))
              beautiful.volume.update()
            end,
            {description = "volume 100%", group = "hotkeys"}),
  awful.key({ altkey, "Control" }, "0", function ()
              os.execute(string.format("amixer -q set %s 0%%", beautiful.volume.channel))
              beautiful.volume.update()
            end,
            {description = "volume 0%", group = "hotkeys"}),
  -- MPD control
  awful.key({ altkey, "Control" }, "Up", function ()
              os.execute("mpc toggle")
              beautiful.mpd.update()
            end,
            {description = "mpc toggle", group = "widgets"}),
  awful.key({ altkey, "Control" }, "Down", function ()
              os.execute("mpc stop")
              beautiful.mpd.update()
            end,
            {description = "mpc stop", group = "widgets"}),
  awful.key({ altkey, "Control" }, "Left", function ()
              os.execute("mpc prev")
              beautiful.mpd.update()
            end,
            {description = "mpc prev", group = "widgets"}),
  awful.key({ altkey, "Control" }, "Right", function ()
              os.execute("mpc next")
              beautiful.mpd.update()
            end,
            {description = "mpc next", group = "widgets"}),
  awful.key({ altkey }, "0", function ()
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
  -- Copy primary to clipboard (terminals to gtk)
  awful.key({ modkey }, "c", function () awful.spawn.with_shell("xsel | xsel -i -b") end,
            {description = "copy terminal to gtk", group = "hotkeys"}),
  -- Copy clipboard to primary (gtk to terminals)
  awful.key({ modkey }, "v", function () awful.spawn.with_shell("xsel -b | xsel") end,
            {description = "copy gtk to terminal", group = "hotkeys"}),
  -- Menubar
  awful.key({ modkey }, "p", function() menubar.show() end,
            {description = "show the menubar", group = "launcher"}),
  -- dmenu
  awful.key({ modkey }, "x", function ()
              os.execute(string.format("dmenu_run -i -fn 'Monospace' -nb '%s' -nf '%s' -sb '%s' -sf '%s'",
              beautiful.bg_normal, beautiful.fg_normal, beautiful.bg_focus, beautiful.fg_focus))
            end,
            {description = "show dmenu", group = "launcher"}),
  -- Mostrar menú
  awful.key({ modkey,           }, "w", function () awful.util.mymainmenu:show() end,
            {description = "show main menu", group = "awesome"}),
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

clientkeys = my_table.join(
  -- Mueve al cliente en estado urgente
  awful.key({ modkey }, "u",
    awful.client.urgent.jumpto,
    {description = "Mover a cliente urgente", group = "client"}),
  awful.key({ modkey, "Shift" }, "m",
    lain.util.magnify_client,
    {description = "Maximizar cliente", group = "client"}),
  awful.key({ modkey, "Shift" }, "x", function(c)
    c:kill()
  end, {description = "Cerrar cliente", group = "client"}),
  awful.key({ altkey }, "space",
    awful.client.floating.toggle,
    {description = "Floating", group = "client"}),
  awful.key({ modkey, "Control" }, "Return", function(c)
    c:swap(awful.client.getmaster())
  end, {description = "Mover a master", group = "client"}),
  awful.key({ modkey }, "o", function(c)
    c:move_to_screen()
  end, {description = "Mover a pantalla", group = "client"}),
  awful.key({ modkey }, "t",
    function (c) c.ontop = not c.ontop
  end, {description = "Mantener al frente", group = "client"}),
  awful.key({ modkey }, "n", function(c)
    c.minimized = true
  end, {description = "Minimizar", group = "client"}),
  awful.key({ modkey }, "m", function(c)
    c.maximized = not c.maximized
    c:raise()
  end, {description = "Maximizar", group = "client"})
)

-- Asigna espacios a teclas de numeros
-- for i = 1, 5 do
--   -- Crea entradas en pantalla de ayuda
--   local descr_view, descr_toggle, descr_move, descr_toggle_focus
--   if i == 1 or i == 5 then
--     descr_view = {description = "Ver #", group = "tag"}
--     descr_toggle = {description = "Seleccionar #", group = "tag"}
--     descr_move = {description = "Mover cliente a #", group = "tag"}
--     descr_toggle_focus = {description = "Mostrar cliente en #", group = "tag"}
--   end
--   local j = i + 4
--   globalkeys = my_table.join(globalkeys,
--     -- Moverse a un espacio.
--     awful.key({ modkey }, "#" .. i + 9, function()
--       local screen = awful.screen.focused()
--       local tag = screen.tags[j]
--       if tag then
--         tag:view_only()
--       end
--     end, descr_view),
--     -- Selecciona varios espacios
--     awful.key({ modkey, "Control" }, "#" .. i + 9, function()
--       local screen = awful.screen.focused()
--       local tag = screen.tags[j]
--       if tag then
--         awful.tag.viewtoggle(tag)
--       end
--     end, descr_toggle),
--     -- Mueve cliente a un espacio
--     awful.key({ modkey, "Shift" }, "#" .. i + 9, function()
--       if client.focus then
--         local tag = client.focus.screen.tags[j]
--         if tag then
--           client.focus:move_to_tag(tag)
--         end
--       end
--     end, descr_move),
--     -- Muestra el cliente activo en un espacio, funciona para no tener que mover un cliente solo muestra duplicado
--     awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9, function()
--       if client.focus then
--         local tag = client.focus.screen.tags[j]
--         if tag then
--           client.focus:toggle_tag(tag)
--         end
--       end
--     end, descr_toggle_focus)
--   )
-- end

-- Asigna espacios a las letras
-- local t = {'a', 's', 'd', 'f'}
-- for i = 1, 4 do
--   local j = t[i]
--   globalkeys = my_table.join(globalkeys,
--   -- Moverse a un espacio.
--   awful.key({ modkey }, j, function ()
--     local screen = awful.screen.focused()
--     local tag = screen.tags[i]
--     if tag then
--       tag:view_only()
--     end
--   end, descr_view),
--   -- Selecciona varios espacios
--   awful.key({ modkey, "Control" }, j, function()
--     local screen = awful.screen.focused()
--     local tag = screen.tags[i]
--     if tag then
--       awful.tag.viewtoggle(tag)
--     end
--   end, descr_toggle),
--   -- Mueve cliente a espacio
--   awful.key({ modkey, "Shift" }, j, function()
--     if client.focus then
--       local tag = client.focus.screen.tags[i]
--       if tag then
--         client.focus:move_to_tag(tag)
--       end
--     end
--   end, descr_move),
--   -- Muestra cliente en espacio
--   awful.key({ modkey, "Control", "Shift" }, j, function()
--     if client.focus then
--     local tag = client.focus.screen.tags[i]
--       if tag then
--         client.focus:toggle_tag(tag)
--       end
--     end
--   end, descr_toggle_focus)
--   )
-- end

-- local arr = {'8', '9', '0'}
-- for i = 1, 3 do
--   local j = arr[i]
--   local k = 9 + i
--   globalkeys = my_table.join(globalkeys,
--   -- Moverse a un espacio.
--   awful.key({ modkey }, j, function()
--     local screen = awful.screen.focused()
--     local tag = screen.tags[k]
--     if tag then
--       tag:view_only()
--     end
--   end, descr_view),
--   -- Selecciona varios espacios
--   awful.key({ modkey, "Control" }, j, function()
--     local screen = awful.screen.focused()
--     local tag = screen.tags[k]
--     if tag then
--       awful.tag.viewtoggle(tag)
--     end
--   end, descr_toggle),
--   -- Mueve cliente a espacio
--   awful.key({ modkey, "Shift" }, j, function()
--     if client.focus then
--       local tag = client.focus.screen.tags[k]
--       if tag then
--         client.focus:move_to_tag(tag)
--       end
--     end
--   end, descr_move),
--   -- Muestra cliente en espacio
--   awful.key({ modkey, "Control", "Shift" }, j, function()
--     if client.focus then
--       local tag = client.focus.screen.tags[k]
--       -- local tag = screen.tags[k]
--       if tag then
--         client.focus:toggle_tag(tag)
--       end
--     end
--   end, descr_toggle_focus)
--   )
-- end

for i = 1, 12 do
  j = awful.util.ws_keys[i]

  globalkeys = my_table.join(globalkeys,
  -- Moverse a un espacio.
  awful.key({ modkey }, j, function()
    local screen = awful.screen.focused()
    local tag = screen.tags[i]
    if tag then
      tag:view_only()
    end
  end, descr_view),
  -- Selecciona varios espacios
  awful.key({ modkey, "Control" }, j, function()
    local screen = awful.screen.focused()
    local tag = screen.tags[i]
    if tag then
      awful.tag.viewtoggle(tag)
    end
  end, descr_toggle),
  -- Mueve cliente a espacio
  awful.key({ modkey, "Shift" }, j, function()
    if client.focus then
      local tag = client.focus.screen.tags[i]
      if tag then
        client.focus:move_to_tag(tag)
      end
    end
  end, descr_move),
  -- Muestra cliente en espacio
  awful.key({ modkey, "Control", "Shift" }, j, function()
    if client.focus then
      local tag = client.focus.screen.tags[i]
      -- local tag = screen.tags[k]
      if tag then
        client.focus:toggle_tag(tag)
      end
    end
  end, descr_toggle_focus)
  )
end

clientbuttons = awful.util.table.join(
  awful.button({ }, 1, function(c)
    c:emit_signal("request::activate", "mouse_click", {raise = true})
  end),
  -- Cambiar posición del cliente
  awful.button({ modkey }, 1, function(c)
    c:emit_signal("request::activate", "mouse_click", {raise = true})
    awful.mouse.client.move()
  end),
  -- Cambiar tamaño del cliente
  awful.button({ modkey }, 3, function(c)
    c:emit_signal("request::activate", "mouse_click", {raise = true})
    awful.mouse.client.resize()
  end)
)

-- Establecer las keybindings
root.keys(globalkeys)

-- Menu
-------
local myawesomemenu = {
  { "hotkeys", function() return false, hotkeys_popup.show_help end },
  { "manual", terminal .. " -e man awesome" },
  { "edit config", string.format("%s -e %s %s", terminal, editor, awesome.conffile) },
  { "restart", awesome.restart },
  { "quit", function() awesome.quit() end }
}
awful.util.mymainmenu = freedesktop.menu.build({
  icon_size = beautiful.menu_height or dpi(25),
  before = {
    { "Awesome", myawesomemenu, beautiful.awesome_icon },
  },
  after = {
    { "Open terminal", terminal },
  }
})

-- Hides the menu if the mouse leave it
-- awful.util.mymainmenu.wibox:connect_signal("mouse::leave", function() awful.util.mymainmenu:hide() end)
-- Applies the default terminal emulator for apps that need it
menubar.utils.terminal = terminal

-- Screens
----------
-- Reloads the Wallpaper
screen.connect_signal("property::geometry", function(s)
  if beautiful.wallpaper then
    local wallpaper = beautiful.wallpaper
    if type(wallpaper) == "function" then
      wallpaper = wallpaper(s)
    end
    gears.wallpaper.maximized(wallpaper, s, "#ffffff", 1)
  end
end)

-- No border when only one client is opened
screen.connect_signal("arrange", function(s)
  local only_one = #s.tiled_clients == 1
  for _, c in pairs(s.clients) do
    if only_one and not c.floating or c.maximized then
      c.border_width = 0
    else
      c.border_width = beautiful.border_width
    end
  end
end)

-- Creates the bars (wibar)
awful.screen.connect_for_each_screen(function(s)
  beautiful.at_screen_connect(s)
end)

-- Deletes the wibar
awful.screen.disconnect_for_each_screen(function()
  beautiful.at_screen_connect(s)
end)

-- Clients Rules
----------------
awful.rules.rules = {
  -- Rules for all the clients
  { rule = { },
    properties = {
      border_width = beautiful.border_width,
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
    properties = { tag = "4", floating = true, titlebars_enabled = false, maximized = false } },

  -- Gimp
  { rule = { class = "Gimp", role = "gimp-image-window" },
    properties = { maximized = true } },
}

-- For news clients
client.connect_signal("manage", function (c)
  if not awesome.startup then awful.client.setslave(c) end
  -- Stablish a new client as a child if there is another client of the same application
  if awesome.startup and
    not c.size_hints.user_position
    and not c.size_hints.program_position then
      -- Prevent clients from being unreachable after screen count changes.
      awful.placement.no_offscreen(c)
  end
end)

-- Format of the titlebar, if enabled
client.connect_signal("request::titlebars", function(c)
  -- Personalized
  if beautiful.titlebar_fun then
    beautiful.titlebar_fun(c)
    return
  end

  -- Default
  -- Actions with the mouse on the titlebar
  local buttons = my_table.join(
    -- Move clients with the mouse
    awful.button({ }, 1, function()
      c:emit_signal("request::activate", "titlebar", {raise = true})
      awful.mouse.client.move(c)
    end),
    -- Kill client with central click
    awful.button({ }, 2, function() c:kill() end),
    -- Resize client
    awful.button({ }, 3, function()
      c:emit_signal("request::activate", "titlebar", {raise = true})
      awful.mouse.client.resize(c)
    end)
  )

  -- Layout of the elements
  awful.titlebar(c, {size = dpi(16)}) : setup {
    { -- Left
      awful.titlebar.widget.iconwidget(c),
      buttons = buttons,
      layout  = wibox.layout.fixed.horizontal
    },
    { -- Middle
      { -- Name
        align  = "center",
        widget = awful.titlebar.widget.titlewidget(c)
      },
      buttons = buttons,
      layout  = wibox.layout.flex.horizontal
    },
    { -- Right
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

-- Activate sloppy focus, so focus follows the mouse
client.connect_signal("mouse::enter", function(c)
  c:emit_signal("request::activate", "mouse_enter", {raise = vi_focus})
end)

-- Change color of the client border
client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

-- EOF
