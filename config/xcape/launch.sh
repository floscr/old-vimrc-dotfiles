#!/usr/bin/env bash

pkill -9 -f xcape

# xset r rate 180 120

# xmodmap -e "clear lock"
# xmodmap -e "keycode 66 = Super_L"
# xmodmap -e "keycode 36 = Super_R"


# spare_modifier="Hyper_L"
# xmodmap -e "keycode 65 = $spare_modifier"
# xmodmap -e "remove mod4 = $spare_modifier"
# xmodmap -e "add Control = $spare_modifier"

# xmodmap -e "keycode any = space"
# xmodmap -e "keycode any = Return"

# xcape -t 1000 -e "Shift_L=Escape;Shift_R=Escape;Super_R=Return;$space_modifier=space"

setxkbmap -option ctrl:nocaps
xcape -e 'Control_L=Escape'
