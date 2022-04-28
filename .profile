# Add node global applications to $PATH
export PATH=$PATH:$HOME/.node/bin
export PATH=$PATH:$HOME/.local/bin

# Add GPG key to shell profile
export GPG_TTY=$(tty)

# Add ripgrep as default fzf command
if type rg &> /dev/null; then
	export FZF_DEFAULT_COMMAND='rg --files'
	export FZF_DEFAULT_OPTS='-m --height 50% --border'
fi

# Add ssh-agent (started through systemd/User)
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
