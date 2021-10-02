# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

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
  zinit light romkatv/powerlevel10k

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
  bindkey "^[[4~"   end-of-line        #END (st)
  bindkey "^[[1;5C" forward-word       #CTRL + RIGHT ARROW
  bindkey "^[[1;5D" backward-word      #CTRL + LEFT ARROW
  bindkey "^H"      backward-kill-word #CTRL + BACKSPACE
  bindkey "^[[3;5~" kill-word          #CTRL + DELETE

### Custom aliases
  alias ll="ls -l"

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

  function cowsayfortune {
      NUMOFCOWS=`cowsay -l | tail -n +2 | wc -w`
      WHICHCOW=$((RANDOM%$NUMOFCOWS+1))
      THISCOW=`cowsay -l | tail -n +2 | sed -e 's/\ /\'$'\n/g' | sed $WHICHCOW'q;d'`

      #echo "Selected cow: ${THISCOW}, from ${WHICHCOW}"
      fortune | cowsay -f $THISCOW -W 100
  }

  function node_module_folders {
      mkdir infra
      mkdir infra/http
      mkdir infra/http/controllers
      mkdir infra/http/routers
      mkdir infra/typeorm
      mkdir infra/typeorm/entities
      mkdir infra/typeorm/repositories
      mkdir repositories
      mkdir services
  }

### To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
typeset -g POWERLEVEL9K_INSTANT_PROMPT=quiet
