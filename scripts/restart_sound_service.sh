#!/usr/bin/env bash

function exit_error {
  echo "FATAL: $@" >&2
  notify-send -i dialog-error "$@"
  exit 1
}

function warning {
  echo "WARNING: $@" >&2
  notify-send -i dialog-warning "$@"
  return 1
}

function notify_ok {
  echo "SUCCESS: $@"
  notify-send -i emblem-ok-symbolic "$@"
  return 0
}

notify-send -i audio-headset "Restarting Audio Services."
echo "1. Stopping PipeWire and WirePlumber..."
systemctl --user stop pipewire.socket pipewire-pulse.socket wireplumber || \
  exit_error "Unable to stop PipeWire and/or WirePlumber."
echo "(1/6) Done."

sleep 1

echo "2. Resetting ALSA kernel state..."
sudo alsactl init  || warning "Unable to reset ALSA kernel state."
echo "(2/6) Done."

sleep 1

echo "3. Reloading USB sound driver modules..."
sudo modprobe -r snd_usb_audio && sudo modprobe snd_usb_audio || \
  warning "Unable to reload USB sound driver modules."
echo "(3/6) Done."

sleep 1

echo "4. Restarting PipeWire and WirePlumber..."
systemctl --user restart pipewire pipewire-pulse wireplumber || \
  exit_error "Unable to restart PipeWire and/or WirePlumber"
echo "(4/6) Done."

sleep 1

# This actually waits for wireplumber to register the devices.
echo "5. Audio stack has been reset. Checking status..." 
wpctl status | grep -A 5 "Sources" || \
  warning "Audio devices not listed properly in wpctl"
echo "(5/6) Done."

# Forces the USB Condenser Mic (ID 61) to be the default.
# wpctl set-default 61

# After devices are detected, restart connections to them.
echo "6. Restarting (by killing) Chrome browser Audio Service..."
ps aux | \
  grep "type=utility --utility-sub-type=audio.mojom.AudioService" | \
  grep -v grep | \
  awk '{print $2}' | xargs kill -9 || \
  exit_error "Unable to restart Chrome browser Audio Service."
echo "(6/6) Done."

notify_ok "Done Restarting Audio Services."
