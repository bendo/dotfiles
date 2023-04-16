#!/bin/sh

xrdb -merge .Xresources &
urxvtd -q -o -f

xloadimage -onroot -fullscreen ~/.m/bg.png

# unclutter -idle 3 -jitter 5 -noevents -root &
xbanish &

xbindkeys &

xset s off -dpms &
xsetroot -cursor_name left_ptr &

xinput set-prop 14 319 1

eval $(gnome-keyring-daemon --start)
export SSH_AUTH_SOCK

exec xmonad
