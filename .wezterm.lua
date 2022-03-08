local wezterm = require 'wezterm';

local scheme_name = "Hybrid" -- Hybrid, JetBrains Darcula, Tomorrow Night, Molokai, Sundried, Japanesque, NightLion v2, FirefoxDev, Wryan, Monokai Remastered, Hardcore, Teerb, Wombat, Operator Mono Dark, OneHalfDark, Ripped Casts, lovelace
local scheme = wezterm.get_builtin_color_schemes()[scheme_name]
scheme.background = "#121212"

return {
   color_schemes = {
      [scheme_name] = scheme
   },
   color_scheme = scheme_name,
   enable_tab_bar = false,
   window_decorations = "NONE",
   -- weight: Thin, ExtraLight, Light, Regular, Medium, SemiBold, Bold, ExtraBold
   -- stretch: Normal, SemiCondensed, Condensed, ExtraCondensed
   font = wezterm.font_with_fallback({
      {
        family="Azeret Mono",
        weight="Regular",
        stretch="Normal",
        italic=false,
        harfbuzz_features={"calt=0","liga=0"}
      },
      { family="Noto Sans Mono", weight="Regular", stretch="Normal", italic=false },
      { family="Source Code Pro"},
      { family="Consolas"},
      { family="Terminus"}
   }),
   adjust_window_size_when_changing_font_size = false,
   disable_default_key_bindings = true,
   keys = {
      { key="2", mods="CTRL", action = wezterm.action{ SendString="\x00"} },
      { key="PageDown", mods="SHIFT|CTRL", action = wezterm.action { ActivateTabRelative=1 } },
      { key="PageUp", mods="SHIFT|CTRL", action = wezterm.action { ActivateTabRelative=-1 } },
      { key="V", mods="CTRL", action=wezterm.action { PasteFrom="Clipboard" } },
      { key="R", mods="SUPER", action = "ReloadConfiguration" },
      { key = "-", mods="CTRL", action = "DecreaseFontSize" },
      { key = "=", mods="CTRL", action = "IncreaseFontSize" },
      { key = "w", mods="SUPER", action = wezterm.action { CloseCurrentTab = { confirm = true } } },
      { key = "t", mods="SUPER", action = wezterm.action { SpawnTab="CurrentPaneDomain" } },
   }
}
