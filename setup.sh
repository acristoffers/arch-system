#!/usr/bin/env bash

if [[ "$USER" = "root" ]]; then
	echo "Run this script as a normal user."
	echo "To create a user, edit and run create_user.sh"
	exit 0
fi

################################################################################
###                                                                          ###
###                                Setup AUR                                 ###
###                                                                          ###
################################################################################

pushd ~ || exit
git clone https://aur.archlinux.org/yay.git --depth=1
pushd yay || exit
doas pacman -S binutils fakeroot base-devel
makepkg -si
popd || exit
rm -rf yay
popd || exit

################################################################################
###                                                                          ###
###                      Configures shells and (neo)vim                      ###
###                                                                          ###
################################################################################

yay -S fish zsh bash python3 nodejs neovim vim curl which npm python-pip emacs \
	tmux fzf lazygit

mkdir ~/.npm-global
npm config set prefix "$HOME/.npm-global"

pip3 install neovim jedi python-language-server powerline-status
npm -g i neovim

export EDIT=nvim
export DISPLAY=:0

pushd ~ || exit
git clone https://github.com/acristoffers/shell_profile_generator
pushd shell_profile_generator || exit
python3 install.py fish bash zsh
popd || exit
rm -rf shell_profile_generator
popd || exit

curl https://git.io/fisher --create-dirs -sLo ~/.config/fish/functions/fisher.fish
fish -c "fisher install dracula/fish"
fish -c "fisher install jethrokuan/fzf"

cp dotfiles/vimrc ~/.vimrc
cp dotfiles/tmux.conf ~/.tmux.conf

pushd ~ || exit
rm -rf .vim .config/nvim .local/share/nvim &>/dev/null
mkdir -p .config/nvim
pushd .config/nvim || exit
ln -s ~/.vimrc init.vim
popd || exit
popd || exit

NVIM_CFG_FDR=${XDG_DATA_HOME:-$HOME/.local/share}
VIM_PLUG=https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
curl -fLo ~/.vim/autoload/plug.vim --create-dirs $VIM_PLUG
curl -fLo "$NVIM_CFG_FDR"/nvim/site/autoload/plug.vim --create-dirs $VIM_PLUG

nvim --headless +PlugInstall +qa

nvim --headless +'PromptlineSnapshot ~/.promptline.sh airline' +qa
tee ~/.config/fish/functions/fish_prompt.fish <<EOF
function fish_prompt
  env FISH_VERSION=\$FISH_VERSION PROMPTLINE_LAST_EXIT_CODE=\$status bash ~/.promptline.sh left
end
EOF
echo "source ~/.promptline.sh" | tee -a ~/.profile
echo "source ~/.promptline.sh" | tee -a ~/.zshrc
nvim ~/.promptline.sh -c 'argdo /truncation' \
	-c 'argdo normal di"' \
	-c 'argdo /dir_sep' \
	-c 'argdo normal di"' \
	-c 'wq'

if ! grep -Fq fish /etc/shells; then
	which fish | doas tee -a /etc/shells
fi
chsh -s "$(which fish)"

git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
cp -r dotfiles/doom.d ~/.doom.d
~/.emacs.d/bin/doom install

# GO111MODULE=on go get golang.org/x/tools/gopls@latest
# go get -u github.com/nsf/gocode
# go get github.com/fatih/gomodifytags
# go get -u github.com/cweill/gotests/...
# go get -u github.com/motemen/gore/cmd/gore
# go get   golang.org/x/tools/cmd/guru
# go build golang.org/x/tools/cmd/guru
# mv guru "$(go env GOROOT)/bin"

################################################################################
###                                                                          ###
###                           Xmonad and Alacritty                           ###
###                                                                          ###
################################################################################

yay -S nerd-fonts-inconsolata xmonad xmobar nitrogen picom trayer sddm slock \
	alacritty firefox qalculate-gtk openssh xmonad-contrib rsync xclip \
	pipewire alsa-utils paprefs pavucontrol twmn rofi qt5-styleplugins \
    arc-gtk-theme arc-kde arc-icon-theme xdg-user-dirs

xdg-user-dirs-update
mkdir ~/Developer
cp dotfiles/wallpaper.jpg ~/Images/wallpaper.jpg
cp dotfiles/gtkrc-2.0 ~/.gtkrc-2.0
rsync -a dotfiles/config/ ~/.config/
rsync -a dotfiles/xmonad/ ~/.xmonad/

doas systemctl enable sddm
systemctl --user enable pipewire
amixer sset Master unmute
amixer sset Master 80%

echo "All done. If you changed the username, nitrogen will not work automatically. Just reconfigure it."
