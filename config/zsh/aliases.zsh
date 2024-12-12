#!/bin/sh

alias cl="clear"
alias grep="ugrep --color=auto"
alias l="eza -la --color=auto --group-directories-first"
alias ls="ls --color=auto -h"
alias lt="eza --tree --level=2 --git -a"
alias ltree="eza --tree --level=2 --long --git -a"
alias ..="cd .."

alias sync-server="ssh -L 9988:localhost:8384 server"
alias sync-remote="ssh -L 9987:localhost:8384 jenasoff"

alias restart-emacs="killall emacs || echo 'Emacs server not running'; /usr/bin/emacs --daemon" 
alias reload-wifi="sudo rmmod mt7921e && sudo modprobe mt7921e"

alias tr="trash"
alias trr="trash restore"
alias tu="trash restore -r 0" # restore last trashed item

alias ga="cd ~/archive"
alias gb="cd ~/.local/bin"
alias gc="cd ~/.config"
alias gd="cd ~/downloads"
alias g.="cd ~/dotfiles"
alias gf="cd ~/documents"
alias gh="cd"
alias go="cd ~/org"
alias gp="cd ~/pictures"
alias gs="cd ~/pictures/screenshots"
alias gt="cd ~/.local/share/Trash/files"
