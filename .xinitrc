#!/bin/sh

xrdb -merge .Xresources &
urxvtd -q -o -f

xloadimage -onroot -fullscreen ~/.i3/bg.jpg

exec i3
