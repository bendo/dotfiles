#!/bin/sh

xrdb -merge .Xresources &
urxvtd -q -o -f

xloadimage -onroot -fullscreen ~/.i3/h1.png

# unclutter -idle 3 -jitter 5 -noevents -root &
xbanish &

xset s off -dpms&
xsetroot -cursor_name left_ptr &

#exec i3
exec xmonad
