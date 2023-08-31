# Add pnpm global installs to $PATH
export PNPM_HOME=$HOME/.local/share/pnpm
export PATH=$PATH:$PNPM_HOME

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

export EDITOR="nvim"
# export PAGER="most"

export ANDROID_HOME="/home/arthur/Android/Sdk"
