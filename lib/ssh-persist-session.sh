# Save SSH key
# Note: ~/.ssh/environment should not be used, as it already has a
# different purpose in SSH
env=~/.ssh/agent.env

# Note: Don't bother checking SSH_AGENT_PID. It is not used by
# SSH itself, and it might even be incorrect.
# (For example, when using agent-forwarding over SSH)
agent_is_running() {
  if [ "$SSH_AUTH_SOCK" ]; then
    # ssh-add returns:
    # 0 = agent running, has keys
    # 1 = agent running, no keys
    # 2 = agent not running
    ssh-add -l >/dev/null 2>&1 || [ $? -eq 1 ]
  else
    false
  fi
}

agent_has_keys() {
  ssh-add -l > /dev/null 2>&1
}

agent_has_home_keys() {
  ssh-add -l | grep "$HOME/.ssh/" > /dev/null 2>&1
}

agent_load_env() {
  source "$env" > /dev/null
}

agent_start() {
  (umask 077; ssh-agent > "$env")
  source "$env" > /dev/null
}

if ! agent_is_running; then
  echo "Loading SSH environment."
  agent_load_env
fi

if ! agent_is_running; then
  echo "Starting ssh-agent."
  agent_start
  ssh-add
elif ! agent_has_keys; then
  ssh-add
elif ! agent_has_home_keys; then
  ssh-add
fi

unset env
