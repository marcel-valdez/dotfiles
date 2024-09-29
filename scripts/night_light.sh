#!/usr/bin/env bash

# Stop on first failure.
set -e

# First obtain a location code from: https://weather.codes/search/

# Insert your location. For example LOXX0001 is a location code for Bratislava, Slovakia
# MTV, CA, USA
location="USCA0746"
tmpfile="/tmp/${location}.out"
# Declare so exit codes aren't hidden.
SUNR=
SUNS=
sunrise_time=
sunset_time=
current_time=
sunrise_since_epoch=
sunset_since_epoch=
current_since_epoch=


# Obtain sunrise and sunset raw data from weather.com
wget -q "https://weather.com/weather/today/l/${location}" -O "${tmpfile}"

SUNR=$(grep SunriseSunset "${tmpfile}" | grep -oE '((1[0-2]|0?[1-9]):([0-5][0-9]) ?([AaPp][Mm]))' | head -1)
SUNS=$(grep SunriseSunset "${tmpfile}" | grep -oE '((1[0-2]|0?[1-9]):([0-5][0-9]) ?([AaPp][Mm]))' | tail -1)

sunrise_time=$(date --date="${SUNR}" "+%R")
sunset_time=$(date --date="${SUNS}" "+%R")
current_time=$(date "+%R")

sunrise_since_epoch=$(date -d "${sunrise_time}" "+%s")
sunset_since_epoch=$(date -d "${sunset_time}" "+%s" )
current_since_epoch=$(date "+%s")


if [ -n "${DEBUG}" ]; then
  echo "Sunrise for location $location: ${sunrise_time} (${sunrise_since_epoch})"
  echo "Sunset for location $location: ${sunset_time} (${sunset_since_epoch})"
  echo "Current time: ${current_time} (${current_since_epoch})"
fi


get_display() {
  _display=
  cd /tmp/.X11-unix >/dev/null 2>&1
  for x in X*; do
    # return the first *local* display we find
    _display=":${x#X}"
    cd - >/dev/null 2>&1
    break
  done

  if [ -z "${_display}" ]; then
    echo "No display X11 server available." >&2
    exit 1
  fi

  echo "${_display}"
}

turn_on_night_light() {
  if [ -n "${DEBUG}" ]; then
    echo "Turning ON night light"
  fi

  if [ -z "${DISPLAY}" ]; then
    DISPLAY="$(get_display)"
    export DISPLAY
  fi

  "/home/marcelvaldez/.local/bin/xsct" 4000
}


turn_off_night_light() {
  if [ -n "${DEBUG}" ]; then
    echo "Turning OFF night light"
  fi

  if [ -z "${DISPLAY}" ]; then
    DISPLAY="$(get_display)"
    export DISPLAY
  fi

  "/home/marcelvaldez/.local/bin/xsct" 6500
}


main() {
  if [ "${current_since_epoch}" -ge "${sunset_since_epoch}" ] ||
       [ "${current_since_epoch}" -lt "${sunrise_since_epoch}" ]; then
    turn_on_night_light
  else
    turn_off_night_light
  fi
}


if ! (return 0 2>/dev/null); then
  main "$@"
fi
