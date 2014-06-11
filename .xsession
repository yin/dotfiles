#!/bin/bash

# Network systay
nm-applet &
# Sound systray
gnome-sound-applet &
# Brightness, volume, etc. buttons with osd
gnome-settings-daemon &
# Dropbox sync
dropbox start -i &
# Make touchpad clicks ignored while typing
syndaemon &

# This is run, when gnome-session, or something is used
. ~/.xsessionrc

exec i3 --shmlog-size=2614400
