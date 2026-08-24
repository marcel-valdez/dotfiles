#!/usr/bin/env bash

source "${HOME}/scripts/pid_utils.sh"

[[ -z "${LATENCY_CALCULATOR_BIN}" ]] && LATENCY_CALCULATOR_BIN="${HOME}/scripts/latency_calculator.py"

SCRIPT="$(basename $0)"

function usage {
    cat <<EOF
${SCRIPT} [--total_time_sec|-t <seconds>] [--host|-h <host>] [--plot_file|-f <file path>] [--plot_title|-pt <title>] [--live|-l] [--help]

Script to measure ping latency against another host.


total_time_sec: Total time in seconds to run the test. (Default: 60)
host: The host against which to run the test. (Default: www.google.com)
plot_file: Image file onto which to save the test's timeseries plot. (Default: /tmp/<host>_latency_plot.png)
plot_title: Title for the plot. (Default: Latency over time for <site>)
live: Whether to show a plot wit live updates of the test. (Default: false)
help: Shows this help message.
EOF
}


host=www.google.com
total_time_sec=60
samples_file="$(mktemp)"
plot_title=
plot_file=
live_updates=
function parse_args {
  while [ $# -gt 0 ]; do
    arg="$1"
    case $arg in
      --help)
          usage
          exit 0
          ;;
      --host|-h)
          host="$2"
          shift
          ;;
      --total_time_sec|-t)
          total_time_sec="$2"
          shift
          ;;
      --plot_file|-f)
          plot_file="$2"
          shift
          ;;
      --plot_title|-pt)
          plot_title="$2"
          shift
          ;;
      --live|-l)
          live_updates=1
          ;;
      *)
          echo "Unknown parameter: $1" >&2
          usage
          exit 1
          ;;
    esac
    shift
  done

  [[ -z "${plot_file}" ]] && plot_file="/tmp/${host//./_}_latency_plot.png"
  [[ -z "${plot_title}" ]] && plot_title="Latency over time for ${host}"

  echo "host=${host}" >&2
  echo "total_time_sec=${total_time_sec}" >&2
  echo "plot_file=${plot_file}" >&2
  echo "plot_title=${plot_title}" >&2
  echo "live_updates=${live_updates}" >&2
}

function timestamp_sec {
    date "+%s"
}

function ping_site {
    ping -i"0.25" "${host}" | grep --line-buffered -oP "(?<=time=)[0-9\.]+"
}

function make_samples() {
    ping_site >> "${samples_file}"
}

function run_test {
    make_samples &
    analyze_samples &
    sleep "${total_time_sec}s" >&2
    # allow for analyze_samples to keep the report line in text
    echo
    stop
    exit 0
}

function analyze_samples() {
    local analyzer_args=("--plot_file" "${plot_file}" "--plot_title" "${plot_title}")
    if [[ "${live_updates}" ]]; then
        analyzer_args+=("--live")
    fi
    tail -n 1000 -f "${samples_file}" | \
        "${LATENCY_CALCULATOR_BIN}" "${analyzer_args[@]}"
}


function stop {
    kill_children "$$" >&2

    if [[ -e "${samples_file}" ]]; then
        rm "${samples_file}" >&2
    fi
}

function stop_and_exit {
    stop
    exit 1
}

trap stop_and_exit SIGINT

function main {
    parse_args "$@"
    run_test
}

main "$@"
