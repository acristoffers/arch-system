#!/usr/bin/env bash

rm -r ~/.doom.d ~/.xmonad ~/.config/xmobar
cp -r dotfiles/doom.d/ ~/.doom.d
cp -r dotfiles/vimrc ~/.vimrc
cp -r dotfiles/tmux.conf ~/.tmux.conf
cp -r dotfiles/config/alacritty/alacritty.yml ~/.config/alacritty/alacritty.yml
cp -r dotfiles/xmonad/ ~/.xmonad
cp -r dotfiles/config/xmobar/ ~/.config/xmobar

nvim --headless +PlugUpgrade +qa
nvim --headless +PlugUpdate +qa
nvim --headless +CocUpdateSync +qa
nvim --headless +'PromptlineSnapshot! ~/.promptline.sh airline' +qa

nvim ~/.promptline.sh -c 'argdo /truncation' \
                      -c 'argdo normal di"' \
                      -c 'argdo /dir_sep' \
                      -c 'argdo normal di"' \
                      -c 'wq'

curl https://git.io/fisher --create-dirs -sLo ~/.config/fish/functions/fisher.fish
fish -c "fisher add dracula/fish"

~/.emacs.d/bin/doom -y sync
~/.emacs.d/bin/doom -y upgrade