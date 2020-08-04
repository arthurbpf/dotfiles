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

### Enables autocompletion
  autoload -Uz compinit
  compinit
  zstyle ":completion:*" menu select
  setopt COMPLETE_ALIASES
  zstyle ":completion::complete:*" gain-privileges 1

### Plugins
  zinit light zsh-users/zsh-completions
  zinit light zsh-users/zsh-autosuggestions
  zinit light zsh-users/zsh-syntax-highlighting
  zinit light supercrabtree/k

### Theme
  zinit light sindresorhus/pure

### Variables
  # Sets history file
  HISTFILE=$HOME/.zsh_history
  # Sets maximum history file size
  HISTSIZE=50000
  # Sets current session commands to be saved to file
  SAVEHIST=50000
  # Appends into history file
  setopt INC_APPEND_HISTORY
  # Saves only one command if duplicates are found
  setopt HIST_IGNORE_DUPS
  # Adds a timestamp for each entry
  setopt EXTENDED_HISTORY

### Custom binds
  bindkey "^[[3~"   delete-char        #DELETE
  bindkey "^[[H"    beginning-of-line  #HOME
  bindkey "^[[F"    end-of-line        #END
  bindkey "^[[1;5C" forward-word       #CTRL + RIGHT ARROW
  bindkey "^[[1;5D" backward-word      #CTRL + LEFT ARROW
  bindkey "^H"      backward-kill-word #CTRL + BACKSPACE
  bindkey "^[[3;5~" kill-word          #CTRL + DELETE

### Custom aliases
  alias ll="ls -la"
  alias pcu="yay -Syu"

### Custom functions
  function forecast {
    curl wttr.in/$1
  }

  function dictionary {
    curl dict://dict.org/d:$1
  }

  function cheat {
    curl cheat.sh/$1
  }

### Custom scripting
  pfetch
