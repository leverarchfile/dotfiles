# .zshrc

PROMPT='%~ > '

unsetopt BEEP

# add completion functions so compinit can find them
fpath=(/usr/share/zsh/site-functions $fpath)
fpath=(${XDG_DATA_HOME:-$HOME/.local/share}/zsh/site-functions $fpath)

autoload -Uz compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots) # completion for hidden files

HISTFILE="${XDG_CACHE_HOME}/zsh/history"
HISTSIZE=5000
SAVEHIST=$HISTSIZE
setopt inc_append_history			# sync history between shells
setopt hist_ignore_all_dups		# remove any older duplicates when adding a new entry
setopt hist_ignore_space			# ignore lines starting with a space

bindkey -v # vi mode

bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -v '^?' backward-delete-char

# change cursor for different vi modes
function zle-keymap-select {
	case $KEYMAP in
	vicmd) echo -ne '\e[1 q' ;;
	viins | main) echo -ne '\e[5 q' ;;
	esac
}
zle -N zle-keymap-select

echo -ne '\e[5 q'								# beam cursor on startup
precmd() { echo -ne '\e[5 q' ;} # beam shape cursor for new prompt

source $ZDOTDIR/aliases.zsh
source "/usr/share/zsh/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh"
source "/usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh"

bindkey '^ ' autosuggest-accept	# ctrl+space to accept current suggestion

# set up fzf key bindings and fuzzy completion
source <(fzf --zsh)
