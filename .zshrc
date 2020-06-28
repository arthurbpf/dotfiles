# @arthurbpf's .zshrc file
# At present time using zplug as plugin manager

### zplug installation (https://github.com/zplug/zplug)
#if [[ ! -d $HOME/.zplug ]]; then
#  print -P "🌺 Initializing zplug installation..."
#  curl -sL --proto-redir -all,https "https://raw.githubusercontent.com/zplug/installer/master/installer.zsh" | zsh
#
#  while [[ ! -f $HOME/.zplug/init.zsh ]]; 
#  do
#    sleep 1
#  done
#fi

### Load zplug
source $HOME/.zplug/init.zsh

### Make zplug manage itself (zplug update)
  zplug 'zplug/zplug', hook-build:'zplug --self-manage'

### Plugins
  zplug "zsh-users/zsh-completions", as:plugin, depth:1
  zplug "zsh-users/zsh-autosuggestions", as:plugin, depth:1
  
### Syntax Highlighting (Must be last loaded plugin)
  zplug "zsh-users/zsh-syntax-highlighting", defer:2

### Theme
  zplug "denysdovhan/spaceship-prompt", use:spaceship.zsh, from:github, as:theme

### Install
#  if ! zplug check; then
#      zplug install
#  fi

### Load plugins
  zplug load

### Custom aliases
  alias ll="ls -la"
  alias pcu="yay -Syyu"
### Custom scripting
  #Runs keyboard color control script
  sh $HOME/.g512.sh