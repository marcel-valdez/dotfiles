#!/bin/env bash

# Path to this script so Genmon knows what to execute on click
SCRIPT_PATH="$(realpath "$0")"
CLI_TOOL="${HOME}/.cargo/bin/pbpctrl"
TIMEOUT_SECS=10

function get_eq_mode {
  local settings=
  settings="$("${CLI_TOOL}" get eq)"
  if [[ "${settings}" == "[0.00, 0.00, 0.00, 0.00, 0.00]" ]]; then
    mode="default"
  elif [[ "${settings}" == "[5.00, 3.00, 0.00, 0.00, 0.00]" ]]; then
    mode="heavy bass"
  elif [[ "${settings}" == "[2.00, 1.00, 0.00, 0.00, 0.00]" ]]; then
    mode="light bass"
  elif [[ "${settings}" == "[1.00, 1.00, 1.00, 1.00, 1.00]" ]]; then
    mode="balanced"
  elif [[ "${settings}" == "[-1.00, -1.00, 2.00, 3.00, 1.00]" ]]; then
    mode="vocal clarity"
  elif [[ "${settings}" == "[4.00, 1.00, -2.00, 1.00, 4.00]" ]]; then
    mode="custom v-shape"
  else
    mode="Other"
  fi
  echo "${mode}"
}

GET_ANC_STDOUT=$(mktemp)
"${CLI_TOOL}" get anc > "${GET_ANC_STDOUT}" &
GET_ANC_PID=$!

# Setup timeout for the get command.
(
  sleep "${TIMEOUT_SECS}"
  kill -15 "${GET_ANC_PID}" 2>/dev/null
) &
TIMEOUT_KILLER_PID=$!

# Wait for the get to finish (with a timeout)
wait "${GET_ANC_PID}"
GET_ANC_STATUS=$?

# Check if the command was successful.
if [[ "${GET_ANC_STATUS}" -eq 0 ]]; then
    ANC_CURRENT="$(cat "${GET_ANC_STDOUT}")"
    # Clean-up timeout killer.
    rm -f "${GET_ANC_STDOUT}"
    kill -9 "${TIMEOUT_KILLER_PID}" 2>/dev/null
    wait "${TIMEOUT_KILLER_PID}" 2>/dev/null
else  # Do not proceed if get anc fails.
  cat<<EOF
<txt><span underline_color='#b8605a' overline_color='#b8605a' underline='single' overline='single'> 🎧 </span></txt>
<tool>Pixel Buds Pro 2
(Disconnected)</tool>
EOF
  exit 1
fi

SPEECH_CURRENT="$("${CLI_TOOL}" get speech-detection)"
EQ_CURRENT="$(get_eq_mode)"
BATTERY="$(${CLI_TOOL} show battery | tail -2 | xargs echo | sed -E 's/[^0-9]+([0-9]+)[^0-9]+([0-9]+)[^0-9]+/left: \1%, right: \2%/g')"

# ==========================================
# MENU LOGIC (Triggered on click)
# ==========================================
if [[ "$1" == "--menu" ]]; then

    # Main Menu
    MAIN_CHOICE=$(zenity --list \
        --title="Pixel Buds Pro 2" \
        --text="Battery => ${BATTERY}" \
        --column="Option" --column="Setting" \
        "Noise Cancellation" "${ANC_CURRENT}" \
        "Speech Recognition" "${SPEECH_CURRENT}" \
        "Equalizer" "${EQ_CURRENT}" \
        --width=350 --height=330 --cancel-label="Close")

    case "$MAIN_CHOICE" in
        "Noise Cancellation")
            ANC_MODE=$(zenity --list --title="Active Noice Cancellation" \
                --column="Mode" "active" "aware" "adaptive" "off" \
                --width=300 --height=330)
            
            if [[ -n "$ANC_MODE" ]]; then
              (
                ${CLI_TOOL} set anc "$ANC_MODE"
                if [ $? -eq 0 ]; then
                  notify-send -i audio-headphones "Pixel Buds Pro 2" "ANC set to: $ANC_MODE"
                else
                  notify-send -i dialog-error "Pixel Buds Pro 2" "Error: Unable to set ANC set to: $ANC_MODE"
                fi
              ) &>/dev/null & disown
            fi
            ;;
            
        "Speech Recognition")
            SPEECH_MODE=$(zenity --list --title="Speech Detection" \
                --column="State" "false" "true" \
                --width=300 --height=200)
                
            if [[ -n "$SPEECH_MODE" ]]; then
              (
                $CLI_TOOL set speech-detection "$SPEECH_MODE"
                if [ $? -eq 0 ]; then
                  notify-send -i audio-headphones "Pixel Buds Pro 2" "Speech Detection set to: $SPEECH_MODE"
                else
                  notify-send -i dialog-error "Pixel Buds Pro 2" "Error: Unable to set Speech Detection to: $SPEECH_MODE"
                fi
              ) &>/dev/null & disown
            fi
            ;;

        "Equalizer")
            # Present the user-friendly names in the GUI
            EQ_MODE=$(zenity --list --title="Equalizer" \
                --column="Preset" \
                "default" \
                "heavy bass" \
                "light bass" \
                "balanced" \
                "vocal clarity" \
                "custom v-shape" \
                --width=300 --height=250)
                
            if [[ -n "$EQ_MODE" ]]; then
                # Map the string to the 5 positional arguments:
                # <LOW_BASS> <BASS> <MID> <TREBLE> <UPPER_TREBLE>
              case "$EQ_MODE" in
                "default")
                  EQ_ARGS=(0.0 0.0 0.0 0.0 0.0)
                  ;;
                "heavy bass")
                  EQ_ARGS=(5.0 3.0 0.0 0.0 0.0)
                  ;;
                "light bass")
                  EQ_ARGS=(2.0 1.0 0.0 0.0 0.0)
                  ;;
                "balanced")
                  EQ_ARGS=(1.0 1.0 1.0 1.0 1.0)
                  ;;
                "vocal clarity")
                  EQ_ARGS=(-1.0 -1.0 2.0 3.0 1.0)
                  ;;
                "custom v-shape")
                  EQ_ARGS=(4.0 1.0 -2.0 1.0 4.0)
                  ;;
              esac

              # Execute the tool with the expanded arguments
              (
                $CLI_TOOL set eq "${EQ_ARGS[@]}"
                if [ $? -eq 0 ]; then
                  notify-send -i audio-headphones "Pixel Buds Pro 2" "EQ Preset Applied: $EQ_MODE"
                else
                  notify-send -i dialog-error "Pixel Buds Pro 2" "Error: Unable to apply EQ Preset: $EQ_MODE"
                fi
              ) &>/dev/null & disown
            fi
            ;;
    esac
    exit 0
fi

# ==========================================
# GENMON OUTPUT (Rendered in the XFCE Panel)
# ==========================================

cat<<EOF
<txt><span underline_color='#537562' overline_color='#537562' underline='single' overline='single'> 🎧 </span></txt>
<txtclick>${SCRIPT_PATH} --menu</txtclick>
<click>${SCRIPT_PATH} --menu</click>
<tool>Pixel Buds Pro 2
BATT: ${BATTERY}
ANC: ${ANC_CURRENT}
SPEECH: ${SPEECH_CURRENT}
EQ: ${EQ_CURRENT}</tool>
EOF
