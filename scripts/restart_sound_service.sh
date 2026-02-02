#!/usr/bin/env bash

# TODO: Verify the machine uses pipewire, pipewire-pulse and wireplumber.
systemctl --user restart pipewire pipewire-pulse wireplumber
