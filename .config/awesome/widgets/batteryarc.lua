-------------------------------------------------
-- Battery Arc Widget for Awesome Window Manager
-- Shows the battery level of the laptop
-- More details could be found here:
-- https://github.com/streetturtle/awesome-wm-widgets/tree/master/batteryarc-widget

-- @author Pavel Makhov
-- @copyright 2020 Pavel Makhov
-- Modificado por: github.com/luis-barrera
-------------------------------------------------

local awful = require("awful")
local beautiful = require("beautiful")
local naughty = require("naughty")
local wibox = require("wibox")
local watch = require("awful.widget.watch")
local markup = require("markup")

local HOME = os.getenv("HOME")

local widget = {}

local function worker(args)

  local font = args.font or 'Noto Sans 8'
  local arc_thickness = 4
  local show_current_level = true 
  local size = 15
  local timeout = 5

  local main_color = beautiful.applets_fg
  local bg_color = args.bg_color
  local low_level_color = "#e53935"
  local medium_level_color = "#ffac20"
  local charging_color = "#79E7FF"
  -- local default_color = "#000000" 
  local default_color = "#FFFFFF" 

  local warning_msg_title = 'Houston, we have a problem'
  local warning_msg_text = 'Battery is dying'
  local warning_msg_position = 'top_right'
  local warning_msg_icon = HOME .. '/.config/awesome/widgets/images/spaceman.jpg'
  local enable_battery_warning = true

  local place_holder = wibox.widget {
    widget = wibox.widget.textbox(markup.fontfg(font, default_color,  " : "))
  }

  local info = wibox.widget {
    widget = wibox.widget.textbox("")
  }

  local arc = wibox.widget {
    max_value = 100,
    rounded_edge = false,
    thickness = arc_thickness,
    start_angle = 4.71238898, -- 2pi*3/4
    forced_height = size,
    forced_width = size,
    colors = { default_color },
    bg = bg_color,
    border_width = 0,
    border_color = default_color,
    widget = wibox.container.arcchart
  }

  local separator = wibox.widget {
    font = font,
    widget = wibox.widget.textbox(" ")
  }

  local last_battery_check = os.time()

  local function update_widget(widget, stdout)
    local charge = 0
    local status
    for s in stdout:gmatch("[^\r\n]+") do
      local cur_status, charge_str, time = string.match(s, '.+: (%a+), (%d?%d?%d)%%,?(.*)')
      if cur_status ~= nil and charge_str ~=nil then
        local cur_charge = tonumber(charge_str)
        if cur_charge > charge then
          status = cur_status
          charge = cur_charge
        end
      end
    end
    arc.value = charge

    if status == 'Charging' then
      arc.bg = charging_color
      info:set_markup_silently(markup.fontfg(font, default_color,  "Chg "))
    elseif status == 'Discharging' then
      arc.bg = bg_color
      info:set_markup_silently(markup.fontfg(font, default_color,  "Dis "))
    else
      arc.colors = { charging_color }
      info:set_markup_silently(markup.fontfg(font, default_color,  "F "))
    end

    if charge < 15 then
      arc.colors = { low_level_color }
      if enable_battery_warning and status ~= 'Charging' and os.difftime(os.time(), last_battery_check) > 60 then
        last_battery_check = os.time()
        show_battery_warning()
      end
    elseif charge >= 15 and charge < 40 then
      arc.colors = { medium_level_color }
    elseif status ~= 'Unknown' then
      arc.colors = { default_color }
    end
  end
  watch("acpi", timeout, update_widget, wibox.widget)

  local widget = wibox.widget {
    place_holder,
    info,
    arc,
    separator,
    layout = wibox.layout.fixed.horizontal
  }

  -- Popup with battery info
  local notification
  function show_battery_status()
    awful.spawn.easy_async([[bash -c 'acpi']],
    function(stdout, _, _, _)
      naughty.destroy(notification)
      notification = naughty.notify {
        text = stdout,
        title = "Battery status",
        timeout = 5,
        hover_timeout = 0.5,
        width = 300,
      }
    end)
  end
  
  --[[ Show warning notification ]]
  function show_battery_warning()
    naughty.notify {
      icon = warning_msg_icon,
      icon_size = 100,
      text = warning_msg_text,
      title = warning_msg_title,
      timeout = 25, -- show the warning for a longer time
      hover_timeout = 0.5,
      position = warning_msg_position,
      bg = "#F06060",
      fg = "#EEE9EF",
      width = 300,
      }
  end
  
  widget:connect_signal("mouse::enter", function()
    show_battery_status()
  end)
  widget:connect_signal("mouse::leave", function()
    --os.execute("sleep 1")
    naughty.destroy(notification)
  end)
  return widget
end

return setmetatable(widget, { __call = function(_, ...)
  return worker(...)
end })
