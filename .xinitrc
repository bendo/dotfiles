#!/bin/sh

xrdb -merge .Xresources &
urxvtd -q -o -f

xloadimage -onroot -fullscreen ~/.i3/bg.jpg

unclutter -idle 3 -jitter 5 -noevents -root &

exec i3
