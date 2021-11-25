#!/usr/bin/env bash


function get_children_pids() {
    local children=$(ps -o pid= --ppid "$1")
    for pid in ${children}; do
        get_children_pids "${pid}"
    done

    if [[ "${children}" ]]; then
        echo "${children}"
    fi
}

function kill_children() {
    local parent="$1"
    shift
    for child in $(get_children_pids "${parent}"); do
        if kill -0 "${child}" &>/dev/null; then
            if [[ "$#" -gt 0 ]]; then
                kill "$@" "${child}"
            else
                kill "${child}"
            fi
        fi
    done
}
