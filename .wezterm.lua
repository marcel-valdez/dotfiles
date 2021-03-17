local wezterm = require 'wezterm';

return {
   color_scheme = "Darkside",
   enable_tab_bar = false,
   window_decorations = "RESIZE",
   font = wezterm.font_with_fallback(
      { "Source Code Pro", "Consolas", "Terminus" }
   ),
   adjust_window_size_when_changing_font_size = false
}
