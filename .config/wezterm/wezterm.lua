
-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

-- This is where you actually apply your config choices

-- For example, changing the color scheme:
config.color_scheme = 'Solarized (light) (terminal.sexy)'


config.font = wezterm.font_with_fallback { 'FixedsysExcelsior Nerd Font', 'Noto Color Emoji' }
config.font_size = 20

config.front_end = "WebGpu"

config.visual_bell = {
  fade_in_function = 'EaseIn',
  fade_in_duration_ms = 150,
  fade_out_function = 'EaseOut',
  fade_out_duration_ms = 150,
}
config.colors = {
  visual_bell = '#202020',
}
local act = wezterm.action

config.mouse_bindings = {
  -- Scrolling up while holding CTRL increases the font size
  {
    event = { Down = { streak = 1, button = { WheelUp = 1 } } },
    action = act.ScrollByLine(-1),
  },

  -- Scrolling down while holding CTRL decreases the font size
  {
    event = { Down = { streak = 1, button = { WheelDown = 1 } } },
    action = act.ScrollByLine(1),
  },
}

-- and finally, return the configuration to wezterm
return config
