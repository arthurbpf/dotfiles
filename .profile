# Add $HOME/bin to $PATH
export PATH=$PATH:$HOME/bin

# Add node global applications to $PATH
export PATH=$PATH:$HOME/.node/bin

# Add GPG key to shell profile
export GPG_TTY=$(tty)

# Add ripgrep as default fzf command
if type rg &> /dev/null; then
	export FZF_DEFAULT_COMMAND='rg --files'
	export FZF_DEFAULT_OPTS='-m --height 50% --border'
fi
