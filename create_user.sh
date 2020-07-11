#!/usr/bin/env bash

USERNAME=alan

echo "Installing sudo"
pacman -Sy sudo

echo "Creating user $USERNAME"
useradd -m $USERNAME
usermod -aG wheel,games,users,uucp,rfkill,mail,sudo $USERNAME

echo "Set $USERNAME password"
passwd $USERNAME
