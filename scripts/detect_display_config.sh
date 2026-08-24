#!/usr/bin/env bash

# TODO: Determine what gdctl show returns when connected to the large Monitor
#       in Maria's house and the portable monitor, and set a configuration
#       accordingly.


# Check if one of the HGC monitor configuratons got detected.
hgc_left_monitor_indices=(1 3 5 7 9 11 13 15)
displays_config="$(gdctl show)"
hgc_monitor_config_detected=
for hgc_left_monitor_index in "${hgc_left_monitor_indices[@]}"; do
  hgc_right_monitor_index="$((hgc_left_monitor_index+1))"
  if echo "${displays_config}" | grep -E "DP-${hgc_left_monitor_index}.*HGC.*24" >/dev/null && \
    echo "${displays_config}" | grep -E "DP-${hgc_right_monitor_index}.*HGC.*24" >/dev/null; then
    hgc_monitor_config_detected=1
    left_monitor="DP-${hgc_left_monitor_index}"
    right_monitor="DP-${hgc_right_monitor_index}"
    # Currently Laptop Only -> Switch to Docked
    gdctl set --persistent \
      --logical-monitor --monitor "${left_monitor}" --mode 1920x1080@144.001 --x 0 --y 0 --scale 1.0 \
      --logical-monitor --primary --monitor "${right_monitor}" --mode 1920x1080@143.994 --x 1920 --y 0 --scale 1.0

    notify-send -i computer "Display Mode" "Configured left (${left_monitor}) and right (${right_monitor}) HGC 24\" monitors: 1920x1080@144Hz"
    exit 0
  fi
done

# Currently Docked -> Switch to Laptop Only
gdctl set --persistent \
    --logical-monitor --primary --monitor eDP-1 --mode 1920x1200@60.003 --x 0 --y 0 --scale 1.25
notify-send -i computer "Display Mode" "Configured built-in laptop monitor (eDP-1): 1920x1200@60Hz"
