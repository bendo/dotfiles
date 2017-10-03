#!/bin/sh

xrdb -merge .Xresources &
urxvtd -q -o -f

xloadimage -onroot -fullscreen ~/.i3/bg.jpg

# unclutter -idle 3 -jitter 5 -noevents -root &
xbanish &

xsetroot -cursor_name left_ptr &

#exec i3
exec xmonad
