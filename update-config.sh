#!/usr/bin/env bash

rm -r ~/.doom.d ~/.config/xmobar
rsync -a dotfiles/doom.d/ ~/.doom.d
rsync -a dotfiles/vimrc ~/.vimrc
rsync -a dotfiles/tmux.conf ~/.tmux.conf
rsync -a dotfiles/gtkrc-2.0 ~/.gtkrc-2.0
rsync -a dotfiles/config/ ~/.config

if [ -f ~/.xmonad/xmonad.hs ]; then
  cp dotfiles/xmonad/xmonad.hs ~/.xmonad/xmonad.hs
else
  cp dotfiles/xmonad/xmonad.hs ~/.xmonad/config.hs
fi

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
fish -c "fisher install dracula/fish"
fish -c "fisher install jethrokuan/fzf"

~/.emacs.d/bin/doom -y sync
~/.emacs.d/bin/doom -y upgrade
