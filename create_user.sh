#!/usr/bin/env bash

USERNAME=alan

echo "Installing sudo"
pacman -S doas sudo
echo "permit nopass keepenv :wheel" > /etc/doas.conf
echo "%wheel ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

echo "Creating user $USERNAME"
systemctl enable systemd-homed.service
systemctl start systemd-homed.service
homectl create $USERNAME
usermod -aG wheel,games,users,uucp,rfkill,mail $USERNAME

echo "Log with your newly created user and run setup.sh"
