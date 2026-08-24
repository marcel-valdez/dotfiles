import os
import time
import threading
from kitty.boss import get_boss
from kitty.fast_data_types import add_timer
from kitty.tab_bar import DrawData, ExtraData, TabBarData, draw_title, Screen, as_rgb

# Powerline symbols
SEPARATOR = ""
THEME_BG = as_rgb(0x1c1c1c)
REFRESH_TIME = 2.0
timer_id = None

def get_cpu_temp():
    for i in range(5):
        path = f"/sys/class/thermal/thermal_zone{i}/temp"
        if os.path.exists(path):
            try:
                with open(path, "r") as f:
                    t = int(f.read().strip()) / 1000
                    if 30 < t < 105: return t
            except: continue
    return None

def get_temp_color(temp):
    t = max(30, min(90, temp))
    ratio = (t - 30) / (90 - 30)
    r = int(0 + (170 - 0) * ratio)
    g = int(255 + (30 - 255) * ratio)
    b = int(0 + (30 - 0) * ratio)
    return (r << 16) | (g << 8) | b

def _redraw_tab_bar(_) -> None:
    tm = get_boss().active_tab_manager
    if tm:
        tm.mark_tab_bar_dirty()

def draw_tab(
    draw_data: DrawData, screen: Screen, tab: TabBarData,
    before: int, max_title_length: int, index: int, is_last: bool,
    extra_data: ExtraData
) -> int:
    global timer_id
    if timer_id is None:
        timer_id = add_timer(_redraw_tab_bar, REFRESH_TIME, True)
    # 1. set colors based on active/inactive state
    if tab.is_active:
        bg = as_rgb(0x555555) # dark gray for active
        fg = as_rgb(0xffffff) # white text
        next_bg = THEME_BG # black (the bar background)
    else:
        bg = as_rgb(0x333333) # simpler gray for inactive
        fg = as_rgb(0xaaaaaa) # dimmer text
        next_bg = THEME_BG

    # 2. draw the tab content
    screen.cursor.bg = bg
    screen.cursor.fg = THEME_BG
    screen.draw(SEPARATOR)
    screen.cursor.bg = bg
    screen.cursor.fg = fg
    screen.draw(f" {tab.title} ")

    # 3. draw powerline separator
    # foreground = current tab bg, background = bar bg (black)
    screen.cursor.fg = bg
    screen.cursor.bg = next_bg
    screen.draw(SEPARATOR)

    # reset for spacing
    screen.cursor.bg = THEME_BG
    screen.draw(" ")

    end_x = screen.cursor.x

    # 4. draw temperature on the right
    if is_last:
        temp = get_cpu_temp()
        if temp is not None:
            temp_text = f" {int(temp)}°c "
            target_x = screen.columns - len(temp_text)

            screen.cursor.x = target_x
            screen.cursor.fg = as_rgb(get_temp_color(temp))
            screen.cursor.bg = THEME_BG
            screen.draw(temp_text)
            end_x = screen.cursor.x


    return end_x
