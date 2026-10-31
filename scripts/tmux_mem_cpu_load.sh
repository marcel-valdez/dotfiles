#!/usr/bin/env bash
# ==============================================================================
# Shell-native tmux-mem-cpu-load
# ==============================================================================
# Drop-in replacement for the compiled thewtex/tmux-mem-cpu-load helper, used
# by tmux_status_daemon.sh where that binary cannot be built or run. On the
# gMac there is no cmake (no Homebrew), and Santa Lockdown blocks unapproved
# locally compiled binaries, so building the plugin is not an option there.
#
# Prints the same memory, CPU and load-average segments as the plugin (same
# metrics, number formats, color tables and powerline glyphs) using only
# OS-provided tools via ~/lib/os_compat.sh. CPU usage has whole-percent
# resolution on macOS (iostat).
#
# Options (the subset of tmux-mem-cpu-load options that is supported):
#   -c, --colors              Use tmux colors in the output.
#   -q, --powerline-right     Use powerline right symbols (implies --colors).
#   -r, --segments-right <N>  Blend the last segment into tmux colour N (0-255).
#   -i, --interval <N>        CPU sampling interval in seconds (default: 1).
#   -h, --help                Print this help.
#
# Usage Example:
#   tmux_mem_cpu_load.sh --colors --interval 1 --powerline-right --segments-right 233
# ==============================================================================

set -uo pipefail

if [[ ! -f "${HOME}/lib/os_compat.sh" ]]; then
  echo "tmux_mem_cpu_load.sh: ${HOME}/lib/os_compat.sh not found" >&2
  exit 1
fi
source "${HOME}/lib/os_compat.sh"

# Powerline "right filled" separator (U+E0B2), as UTF-8 bytes for Bash 3.2.
PWL_RIGHT_FILLED=$'\xee\x82\xb2'
GRAPH_LINES=10

# Background colours (tmux colourN) of the plugin's 101-entry lookup tables
# (common/luts.h), indexed by percentage. The CPU table always uses a
# brightwhite foreground; the memory and load tables switch to black at 50%.
CPU_LUT_BG=(16 16 16 16 16 16 16 17 17 17 17 17 17 17 17 18 18 18 18 18 18 18 19 19 19 19 19 19 20 20 20 20 20 20 20 21 21 21 21 21 21 57 57 57 57 57 57 56 56 56 56 56 56 55 55 55 55 55 55 54 54 54 54 54 54 53 53 53 53 53 53 52 52 52 52 52 52 88 88 88 88 88 88 124 124 124 124 124 124 160 160 160 160 160 160 196 196 196 196 196 196)
MEM_LUT_BG=(16 17 18 18 18 18 18 18 24 24 24 24 24 24 24 24 60 66 66 66 66 66 66 66 66 66 66 66 66 72 72 72 72 72 72 72 72 72 72 72 72 72 72 72 72 71 71 71 107 107 107 107 108 108 108 108 108 144 144 144 144 144 144 144 144 144 144 150 150 186 186 186 180 180 180 180 180 180 180 180 180 180 180 181 181 181 181 187 187 187 187 188 224 224 224 224 224 231 231 231 231)
LOAD_LUT_BG=(16 16 16 16 16 16 16 16 16 17 17 17 59 59 59 59 59 59 59 59 59 59 59 59 59 60 60 60 60 60 60 60 60 60 60 102 102 102 102 102 102 102 102 103 103 103 103 103 103 103 103 103 109 109 109 109 109 109 145 145 145 145 145 145 145 145 146 146 152 152 152 152 152 152 152 152 152 152 152 188 188 188 188 188 188 188 188 188 188 195 195 195 195 231 231 231 231 231 231 231 231)

print_help() {
  sed -n '/^# Options/,/^# Usage Example/p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
}

#######################################
# Clamps a value to an integer percentage index (0-100).
# Arguments:
#   $1 - value
# Outputs:
#   Clamped integer to stdout.
#######################################
clamp_percent() {
  local value="$1"
  [[ "${value}" =~ ^[0-9]+$ ]] || value=0
  (( value > 100 )) && value=100
  printf '%s\n' "${value}"
}

#######################################
# Builds the tmux style of a lookup table entry, e.g.
# "#[fg=brightwhite,bg=colour18]".
# Arguments:
#   $1 - table: cpu, mem or load
#   $2 - index: percentage (0-100)
# Outputs:
#   tmux style string to stdout.
#######################################
lut_style() {
  local table="$1"
  local index
  index="$(clamp_percent "$2")"
  local bg fg="brightwhite"
  case "${table}" in
    cpu) bg="${CPU_LUT_BG[${index}]}" ;;
    mem) bg="${MEM_LUT_BG[${index}]}"; (( index >= 50 )) && fg="black" ;;
    load) bg="${LOAD_LUT_BG[${index}]}"; (( index >= 50 )) && fg="black" ;;
  esac
  printf '#[fg=%s,bg=colour%s]' "${fg}" "${bg}"
}

#######################################
# Emits a powerline right separator in front of a segment: the segment's
# background as foreground, the separator glyph, then the segment style
# (the plugin's powerline(..., POWERLINE_RIGHT)).
# Arguments:
#   $1 - style: segment style from lut_style
# Outputs:
#   tmux format string to stdout.
#######################################
powerline_right() {
  local style="$1"
  local bg="${style##*bg=}"
  bg="${bg%]}"
  printf ' #[fg=%s]%s%s' "${bg}" "${PWL_RIGHT_FILLED}" "${style}"
}

main() {
  local use_colors=0 use_powerline_right=0 segments_to_right=0 right_color=0
  local interval=1

  while (( $# > 0 )); do
    case "$1" in
      -c|--colors) use_colors=1 ;;
      -q|--powerline-right) use_colors=1; use_powerline_right=1 ;;
      -r|--segments-right|-i|--interval)
        if (( $# < 2 )); then
          echo "tmux_mem_cpu_load.sh: option '$1' requires an argument" >&2
          return 1
        fi
        if [[ "$1" == -r || "$1" == --segments-right ]]; then
          if [[ ! "$2" =~ ^[0-9]+$ ]] || (( $2 > 255 )); then
            echo "Valid color values are from 0 to 255." >&2
            return 1
          fi
          segments_to_right=1
          right_color="$2"
        else
          if [[ ! "$2" =~ ^[0-9]+$ ]] || (( $2 < 1 )); then
            echo "Status interval argument must be one or greater." >&2
            return 1
          fi
          interval="$2"
        fi
        shift
        ;;
      --segments-right=*|--interval=*)
        set -- "${1%%=*}" "${1#*=}" "${@:2}"
        continue
        ;;
      -h|--help) print_help; return 1 ;;
      *)
        echo "tmux_mem_cpu_load.sh: unsupported option '$1'" >&2
        return 1
        ;;
    esac
    shift
  done

  # --- Memory: used/total -------------------------------------------------------
  local used_mb=0 total_mb=0 mem_stats
  if mem_stats="$(os::memory_used_total_mb)"; then
    read -r used_mb total_mb <<< "${mem_stats}"
  fi
  local mem_percent=0
  (( total_mb > 0 )) && mem_percent=$(( 100 * used_mb / total_mb ))
  local mem_text
  if (( used_mb > 10000 && total_mb > 10000 )); then
    mem_text="$(( used_mb / 1024 ))/$(( total_mb / 1024 ))GB"
  elif (( used_mb < 10000 && total_mb > 10000 )); then
    mem_text="${used_mb}MB/$(( total_mb / 1024 ))GB"
  else
    mem_text="${used_mb}/${total_mb}MB"
  fi

  # --- CPU: busy percentage over the sampling interval -------------------------
  local cpu_percent
  cpu_percent="$(os::cpu_busy_percent "${interval}")" || cpu_percent=0
  cpu_percent="$(clamp_percent "${cpu_percent}")"
  local bars="" step=0
  local bar_count=$(( cpu_percent * 100 / 999 )) # floor(percent / 99.9 * 10)
  while (( step < GRAPH_LINES )); do
    if (( step < bar_count )); then bars+="|"; else bars+=" "; fi
    step=$(( step + 1 ))
  done
  local cpu_text
  cpu_text="$(printf ' [%s]%6.1f%%' "${bars}" "${cpu_percent}")"

  # --- Load averages -------------------------------------------------------------
  local loads load_ok=0 load1 load5 load15 load_percent=0 ncpu
  if loads="$(os::load_averages)"; then
    load_ok=1
    read -r load1 load5 load15 <<< "${loads}"
    ncpu="$(os::cpu_count)"
    read -r load_percent load1 load5 load15 <<< "$(awk -v n="${ncpu}" \
      -v a="${load1}" -v b="${load5}" -v c="${load15}" 'BEGIN {
        p = int(a / n * 0.5 * 100); if (p > 100) p = 100
        printf "%d %.2f %.2f %.2f\n", p, a, b, c }')"
  fi

  # --- Assemble (same layout as the plugin) -------------------------------------
  local out="" style
  style="$(lut_style mem "${mem_percent}")"
  if (( use_powerline_right )); then
    out+="#[bg=default]$(powerline_right "${style}") ${mem_text}"
  elif (( use_colors )); then
    out+="${style}${mem_text}#[fg=default,bg=default]"
  else
    out+="${mem_text}"
  fi

  style="$(lut_style cpu "${cpu_percent}")"
  if (( use_powerline_right )); then
    out+="$(powerline_right "${style}")${cpu_text}"
  elif (( use_colors )); then
    out+="${style}${cpu_text}#[fg=default,bg=default]"
  else
    out+="${cpu_text}"
  fi

  if (( ! load_ok )); then
    out+=" 0.00 0.00 0.00"
  else
    style="$(lut_style load "${load_percent}")"
    if (( use_powerline_right )); then
      out+="$(powerline_right "${style}") ${load1} ${load5} ${load15}"
      if (( segments_to_right )); then
        out+="${style}#[fg=colour${right_color}] ${PWL_RIGHT_FILLED}${style}"
      fi
    elif (( use_colors )); then
      out+="${style} ${load1} ${load5} ${load15}#[fg=default,bg=default]"
    else
      out+=" ${load1} ${load5} ${load15}"
    fi
  fi

  printf '%s\n' "${out}"
}

main "$@"
