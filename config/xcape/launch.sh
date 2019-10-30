#!/usr/bin/env bash

pkill -9 -f xcape

xset r rate 180 120

# Make space Control L whenn pressed.
spare_modifier="Hyper_L"
xmodmap -e "keycode 65 = $spare_modifier"
xmodmap -e "remove mod4 = $spare_modifier"
xmodmap -e "add Control = $spare_modifier"

# Map space to an unused keycode (to keep it around for xcape to
# use).
xmodmap -e "keycode any = space"

# Finally use xcape to cause the space bar to generate a space when tapped.
xcape -e "$spare_modifier=space"

# Capslock to control
setxkbmap -option ctrl:nocaps
xcape -e 'Control_L=Escape'
