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
cd yay || exit
sudo pacman -S binutils fakeroot base-devel
makepkg -si
popd || exit
rm -rf yay

################################################################################
###                                                                          ###
###                      Configures shells and (neo)vim                      ###
###                                                                          ###
################################################################################

sudo yay -S fish zsh bash python3 nodejs neovim vim curl which

pip3 install neovim jedi python-language-server
npm -g i neovim

pushd ~ || exit
git clone https://github.com/acristoffers/shell_profile_generator
pushd shell_profile_generator || exit
python3 install.py fish bash zsh
popd || exit
rm -rf shell_profile_generator
popd || exit

VIMRC_URL=https://gist.github.com/acristoffers/544347bc4c920527f75a1612db4c689a/raw/.vimrc
curl -L $VIMRC_URL >> ~/.vimrc

pushd ~ || exit
rm -rf .vim .config/nvim .local/share/nvim &> /dev/null
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
tee ~/.config/fish/functions/fish_prompt.fish << EOF
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
    which fish | sudo tee -a /etc/shells
fi
chsh -s "$(which fish)"

