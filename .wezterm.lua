local wezterm = require 'wezterm';

return {
   color_scheme = "FirefoxDev", -- Hybrid, JetBrains Darcula, Tomorrow Night, Molokai, Sundried, Japanesque, NightLion v2, FirefoxDev, Wryan, Monokai Remastered, Hardcore, Teerb, Wombat, Operator Mono Dark, OneHalfDark, Ripped Casts, lovelace, Pencil Dark
   enable_tab_bar = false,
   window_decorations = "RESIZE",
   font = wezterm.font_with_fallback(
      { "Source Code Pro", "Consolas", "Terminus" }
   ),
   adjust_window_size_when_changing_font_size = false,
   keys = {
      { key="2", mods="CTRL", action=wezterm.action{SendString="\x00"} },
      { key="2", mods="SHIFT|CTRL", action="DisableDefaultAssignment" }
   }
}
