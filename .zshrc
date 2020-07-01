# @arthurbpf's .zshrc file
# At present time using Zinit as plugin manager (https://github.com/zdharma/zinit)

### Start of Zinit's installer
if [[ ! -f $HOME/.zinit/bin/zinit.zsh ]]; then
    print -P "%F{33}▓▒░ %F{220}Installing %F{33}DHARMA%F{220} Initiative Plugin Manager (%F{33}zdharma/zinit%F{220})…%f"
    command mkdir -p "$HOME/.zinit" && command chmod g-rwX "$HOME/.zinit"
    command git clone https://github.com/zdharma/zinit "$HOME/.zinit/bin" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
        print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi

source "$HOME/.zinit/bin/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zinit-zsh/z-a-rust \
    zinit-zsh/z-a-as-monitor \
    zinit-zsh/z-a-patch-dl \
    zinit-zsh/z-a-bin-gem-node
### End of Zinit's installation
### To update $ zinit self-update

### Plugins
  zinit light zsh-users/zsh-completions
  zinit light zsh-users/zsh-autosuggestions
  zinit light zsh-users/zsh-syntax-highlighting

### Theme
  zinit light sindresorhus/pure

### Custom binds
  bindkey "^[[3~" delete-char
  bindkey "^[[H"  beginning-of-line
  bindkey "^[[F"  end-of-line

### Custom aliases
  alias ll="ls -la"
  alias pcu="yay -Syu"
### Custom scripting
  #Runs keyboard color control script
  sh $HOME/.g512.sh
