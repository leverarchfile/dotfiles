#!/bin/sh

systemctl --user restart xdg-desktop-portal

/usr/lib/polkit-kde-authentication-agent-1 &

gsettings set "org.gnome.desktop.interface" gtk-theme "Gruvbox-Material-Dark"
gsettings set "org.gnome.desktop.interface" icon-theme "breeze-dark"

kanshi &
waybar &
mako &
udiskie &
foot --server &
kmonad ~/.config/kmonad/config.kbd &
wl-paste --type text --watch cliphist store &
wl-paste --type image --watch cliphist store &
wl-clip-persist --clipboard regular &
nm-applet &
wayland-pipewire-idle-inhibit &
librepods --no-tray &

swayidle -w timeout 300 "lock" timeout 600 "wlopm --off '*'" \
	resume "wlopm --on '*'" before-sleep "lock" &
