### Start of Zplug's installer
if [[ ! -f $HOME/.zplug/init.zsh ]]; then
    print -P "Installing zplug..."
    git clone https://github.com/zplug/zplug $HOME/.zplug
fi

source "$HOME/.zplug/init.zsh"

### Enables autocompletion
  autoload -Uz compinit
  compinit
  zstyle ":completion:*" menu select
  setopt COMPLETE_ALIASES
  zstyle ":completion::complete:*" gain-privileges 1

### Plugins
  zplug "zsh-users/zsh-completions"
  zplug "zsh-users/zsh-autosuggestions"
  zplug "zsh-users/zsh-syntax-highlighting"
  zplug "plugins/git", from:oh-my-zsh
  zplug "plugins/git-auto-fetch", from:oh-my-zsh
  zplug "plugins/git-prompt", from:oh-my-zsh

### Theme
  zplug "romkatv/powerlevel10k", as:theme, depth:1

if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
    echo
fi

zplug load

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
  alias rm="trash"
  alias enw="emacs -nw"
  alias storage="cd /mnt/storage"
  alias sx="startx ~/.xinitrc"

  # Reboot directly to Windows
  # Inspired by http://askubuntu.com/questions/18170/how-to-reboot-into-windows-from-ubuntu
  reboot_to_windows ()
  {
    windows_title=$(doas grep -i windows /boot/grub/grub.cfg | cut -d "'" -f 2)
    doas grub-reboot "$windows_title" && doas reboot
  }
  alias reboot-to-windows="reboot_to_windows"

### Custom functions
  function dictionary {
    curl dict://dict.org/d:$1
  }

  function cowsayfortune {
      NUMOFCOWS=`cowsay -l | tail -n +2 | wc -w`
      WHICHCOW=$((RANDOM%$NUMOFCOWS+1))
      THISCOW=`cowsay -l | tail -n +2 | sed -e 's/\ /\'$'\n/g' | sed $WHICHCOW'q;d'`

      #echo "Selected cow: ${THISCOW}, from ${WHICHCOW}"
      fortune | cowsay -f $THISCOW -W 100
  }

  function wal-tile() {
    wal -ns -i "$@"
    swww img "$(< "${HOME}/.cache/wal/wal")"
  }

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

eval "$(zoxide init zsh)"
