#!/bin/sh

# this script must be run from the root of the dotfiles repository
dotfiles=$(pwd)

# symlink zsh profile
ln -s ${dotfiles}/zprofile ~/.zprofile

# create ~/.config and ~/.local/bin if they don't already exist
mkdir -p ~/.config
mkdir -p ~/.local/bin

# symlink everything in config and local/bin
for file in ${dotfiles}/config/*; do
	ln -s ${file} ~/.config/
done
for file in ${dotfiles}/local/bin/*; do
	ln -s ${file} ~/.local/bin/
done
