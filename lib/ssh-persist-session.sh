# Save SSH key
# Note: ~/.ssh/environment should not be used, as it already has a
# different purpose in SSH
HOME_SSH="$HOME/.ssh"
env="$HOME_SSH/agent.env"

# Note: Don't bother checking SSH_AGENT_PID. It is not used by
# SSH itself, and it might even be incorrect.
# (For example, when using agent-forwarding over SSH)

clear_ssh_add_l_cache() {
  unset SSH_KEYS
  unset SSH_ADD_STATUS
}

clear_ssh_add_l_cache

cache_ssh_add_l() {
  if [ "$SSH_KEYS" == "" ]; then
    SSH_KEYS=$(ssh-add -l)
    SSH_ADD_STATUS=$?
  fi
}

agent_is_running() {
  if [ "$SSH_AUTH_SOCK" ]; then
    cache_ssh_add_l
    # ssh-add returns:
    # 0 = agent running, has keys
    # 1 = agent running, no keys
    # 2 = agent not running
    if [ $SSH_ADD_STATUS -eq 1 ]; then
      false
    else
      true
    fi
  else
    false
  fi
}

agent_has_keys() {
  cache_ssh_add_l
  if [ "$SSH_KEYS" == "" ]; then
    false
  else
    true
  fi
}

agent_has_home_keys() {
  cache_ssh_add_l
  __grep_home_ssh=$(echo "$SSH_KEYS" | grep -o "$HOME_SSH")
  if [ "$__grep_home_ssh" == "$HOME_SSH" ]; then
    true
  else
    false
  fi
}

agent_load_env() {
  source "$env" > /dev/null
}

agent_start() {
  (umask 077; ssh-agent > "$env")
  source "$env" > /dev/null
}


if ! agent_is_running; then
  echo "Loading SSH environment"
  agent_load_env
fi

if ! agent_is_running; then
  echo "Starting ssh-agent"
  agent_start
  ssh-add
else
  if ! agent_has_keys; then
    echo "Adding all keys to ssh-agent"
    ssh-add
  elif ! agent_has_home_keys; then
    echo "Adding keys in $HOME_SSH dir to ssh-agent"
    ssh-add
  fi
fi

unset env
