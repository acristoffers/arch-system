#!/usr/bin/env bash

USERNAME=alan

echo "Installing sudo"
pacman -S sudo

echo "Creating user $USERNAME"
useradd -m $USERNAME
usermod -aG wheel,games,users,uucp,rfkill,mail,sudo $USERNAME
printf "%s\tALL=(ALL:ALL) ALL" $USERNAME >> /etc/sudoers

echo "Set $USERNAME password"
passwd $USERNAME
