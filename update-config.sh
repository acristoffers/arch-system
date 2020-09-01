#!/usr/bin/env bash

rm -r ~/.doom.d ~/.xmonad ~/.config/xmobar
cp -r dotfiles/doom.d/ ~/.doom.d
cp dotfiles/vimrc ~/.vimrc
cp dotfiles/tmux.conf ~/.tmux.conf
cp dotfiles/config/alacritty/alacritty.yml ~/.config/alacritty/alacritty.yml
cp dotfiles/xmonad/ ~/.xmonad
cp dotfiles/config/xmobar/ ~/.config/xmobar

~/.emacs.d/bin/doom -y sync
~/.emacs.d/bin/doom -y upgrade
nvim --headless +PlugUpgrade +qa
nvim --headless +PlugUpdate +qa
nvim --headless +CocUpdateSync +qa

curl https://git.io/fisher --create-dirs -sLo ~/.config/fish/functions/fisher.fish
fish -c "fisher add dracula/fish"
