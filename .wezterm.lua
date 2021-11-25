 local wezterm = require 'wezterm';

return {
   color_scheme = "Hybrid", -- FirefoxDev, Hybrid, JetBrains Darcula, Tomorrow Night, Molokai, Sundried, Japanesque, NightLion v2, FirefoxDev, Wryan, Monokai Remastered, Hardcore, Teerb, Wombat, Operator Mono Dark, OneHalfDark, Ripped Casts, lovelace, Pencil Dark
   enable_tab_bar = false,
   window_decorations = "RESIZE",
   -- weight: Thin, ExtraLight, Light, Regular, Medium, SemiBold, Bold, ExtraBold
   -- stretch: Normal, SemiCondensed, Condensed, ExtraCondensed
   font = wezterm.font_with_fallback({
      { family="Noto Sans Mono", weight="Regular", stretch="Normal", italic=false },
      { family="Source Code Pro"},
      { family="Consolas"},
      { family="Terminus"}
   }),
   adjust_window_size_when_changing_font_size = false,
   keys = {
      { key="2", mods="CTRL", action=wezterm.action{SendString="\x00"} },
      { key="2", mods="SHIFT|CTRL", action="DisableDefaultAssignment" },
      { key="PageDown", mods="SHIFT|CTRL", action=wezterm.action{ ActivateTabRelative=1 } },
      { key="PageUp", mods="SHIFT|CTRL", action=wezterm.action{ ActivateTabRelative=-1 } },
   }
}
