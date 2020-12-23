--[[

     Licensed under GNU General Public License v2
      * (c) 2013,      Luca CPZ
      * (c) 2010-2012, Peter Hofmann

--]]

local naughty = require("naughty")
local wibox   = require("wibox")
local spawn   = require("awful.spawn")
local string  = string

local function factory(args)
  local net        = { widget = wibox.widget.textbox(), devices = {} }
  local args       = args or {}
  local timeout    = args.timeout or 2
  local units      = args.units or 1024 -- KB
  local notify     = args.notify or "on"
  local wifi_state = args.wifi_state or "off"
  local eth_state  = args.eth_state or "off"
  local screen     = args.screen or 1
  local settings   = args.settings or function() end
  local map_table  = {}
  local timer_table = {}

  function newtimer(name, timeout, fun, nostart, stoppable)
    if not name or #name == 0 then return end
    name = (stoppable and name) or timeout
    if not timer_table[name] then
      timer_table[name] = timer({ timeout = timeout })
      timer_table[name]:start()
    end
    timer_table[name]:connect_signal("timeout", fun)
    if not nostart then
      timer_table[name]:emit_signal("timeout")
    end
    return stoppable and timer_table[name]
  end

  function set_map(element, value)
    map_table[element] = value
  end

  function get_map(element)
    return map_table[element]
  end

  -- Compatibility with old API where iface was a string corresponding to 1 interface
  net.iface = (args.iface and (type(args.iface) == "string" and {args.iface}) or
              (type(args.iface) == "table" and args.iface)) or {}

  -- run a command and execute a function on its output line by line
  function line_callback(cmd, callback)
    return spawn.with_line_callback(cmd, {
      stdout = function (line)
      callback(line)
      end,
    })
  end

  -- get first line of a file
  function first_line(path)
    local file, first = io.open(path, "rb"), nil
    if file then
      first = file:read("*l")
      file:close()
    end
    return first
  end

  -- get a table with all lines from a file
  function lines_from(path)
    local lines = {}
    for line in io.lines(path) do
      lines[#lines + 1] = line
    end
    return lines
  end

  function net.get_device()
    line_callback("ip link", function(line)
      net.iface[#net.iface + 1] = not string.match(line, "LOOPBACK") and string.match(line, "(%w+): <") or nil
    end)
  end

  if #net.iface == 0 then net.get_device() end

  function net.update()
    -- These are the totals over all specified interfaces
    net_now = {
      devices  = {},
      -- Bytes since last iteration
      sent     = 0,
      received = 0
    }

    for _, dev in ipairs(net.iface) do
      local dev_now    = {}
      local dev_before = net.devices[dev] or { last_t = 0, last_r = 0 }
      local now_t      = tonumber(first_line(string.format("/sys/class/net/%s/statistics/tx_bytes", dev)) or 0)
      local now_r      = tonumber(first_line(string.format("/sys/class/net/%s/statistics/rx_bytes", dev)) or 0)

      dev_now.carrier  = first_line(string.format("/sys/class/net/%s/carrier", dev)) or "0"
      dev_now.state    = first_line(string.format("/sys/class/net/%s/operstate", dev)) or "down"

      dev_now.sent     = (now_t - dev_before.last_t) / timeout / units
      dev_now.received = (now_r - dev_before.last_r) / timeout / units

      net_now.sent     = net_now.sent + dev_now.sent
      net_now.received = net_now.received + dev_now.received

      dev_now.sent     = string.format("%.1f", dev_now.sent)
      dev_now.received = string.format("%.1f", dev_now.received)

      dev_now.last_t   = now_t
      dev_now.last_r   = now_r

      if wifi_state == "on" and first_line(string.format("/sys/class/net/%s/uevent", dev)) == "DEVTYPE=wlan" and string.match(dev_now.carrier, "1") then
        dev_now.wifi   = true
        dev_now.signal = tonumber(string.match(lines_from("/proc/net/wireless")[3], "(%-%d+%.)")) or nil
      end

      if eth_state == "on" and first_line(string.format("/sys/class/net/%s/uevent", dev)) ~= "DEVTYPE=wlan" and string.match(dev_now.carrier, "1") then
        dev_now.ethernet = true
      end

      net.devices[dev] = dev_now

      -- Notify only once when connection is lost
      if string.match(dev_now.carrier, "0") and notify == "on" and get_map(dev) then
        naughty.notify {
          title    = dev,
          text     = "No carrier",
          icon     = os.getenv("HOME") .. "/.config/awesome/widgets/images/no_net.png",
          screen   = screen
        }
        set_map(dev, false)
      elseif string.match(dev_now.carrier, "1") then
        set_map(dev, true)
      end

        net_now.carrier = dev_now.carrier
        net_now.state = dev_now.state
        net_now.devices[dev] = dev_now
        -- net_now.sent and net_now.received will be
        -- the totals across all specified devices
    end

    net_now.sent = string.format("%.1f", net_now.sent)
    net_now.received = string.format("%.1f", net_now.received)

    widget = net.widget
    settings()
  end

  newtimer("network", timeout, net.update)

  return net
end

return factory
