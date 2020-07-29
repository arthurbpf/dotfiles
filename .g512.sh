#!/bin/bash

#Bus 005 Device 002: ID 046d:c342 Logitech, Inc. G512 SE
#Vendor ID: 046d
#Product ID: c342

#Startup script to define colors
#g512-led -a ddded9

color1=ff4a4a
color2=ba3636
color3=8f2929
color4=7b2323
color5=551818
color6=350f0f


g512-led -kn esc $color1
g512-led -gn fkeys $color1
g512-led -kn printscreen $color1
g512-led -kn scrolllock $color1
g512-led -kn pause_break $color1

g512-led -kn tilde $color2
g512-led -kn 1 $color2
g512-led -kn 2 $color2
g512-led -kn 3 $color2
g512-led -kn 4 $color2
g512-led -kn 5 $color2
g512-led -kn 6 $color2
g512-led -kn 7 $color2
g512-led -kn 8 $color2
g512-led -kn 9 $color2
g512-led -kn 0 $color2
g512-led -kn minus $color2
g512-led -kn equal $color2
g512-led -kn backspace $color2
g512-led -kn insert $color2
g512-led -kn home $color2
g512-led -kn page_up $color2
g512-led -kn num_lock $color2
g512-led -kn num_slash $color2
g512-led -kn num_asterisk $color2
g512-led -kn num_minus $color2

g512-led -kn tab $color3
g512-led -kn q $color3
g512-led -kn w $color3
g512-led -kn e $color3
g512-led -kn r $color3
g512-led -kn t $color3
g512-led -kn y $color3
g512-led -kn u $color3
g512-led -kn i $color3
g512-led -kn o $color3
g512-led -kn p $color3
g512-led -kn open_bracket $color3
g512-led -kn close_bracket $color3
g512-led -kn backslash $color3
g512-led -kn delete $color3
g512-led -kn end $color3
g512-led -kn pagedown $color3
g512-led -kn num7 $color3
g512-led -kn num8 $color3
g512-led -kn num9 $color3
g512-led -kn numplus $color3

g512-led -kn caps_lock $color4
g512-led -kn a $color4
g512-led -kn s $color4
g512-led -kn d $color4
g512-led -kn f $color4
g512-led -kn g $color4
g512-led -kn h $color4
g512-led -kn j $color4
g512-led -kn k $color4
g512-led -kn l $color4
g512-led -kn semicolon $color4
g512-led -kn quote $color4
g512-led -kn enter $color4
g512-led -kn num4 $color4
g512-led -kn num5 $color4
g512-led -kn num6 $color4

g512-led -kn shift_left $color5
g512-led -kn z $color5
g512-led -kn x $color5
g512-led -kn c $color5
g512-led -kn v $color5
g512-led -kn b $color5
g512-led -kn n $color5
g512-led -kn m $color5
g512-led -kn comma $color5
g512-led -kn period $color5
g512-led -kn slash $color5
g512-led -kn shift_right $color5
g512-led -kn arrow_top $color5
g512-led -kn num1 $color5
g512-led -kn num2 $color5
g512-led -kn num3 $color5
g512-led -kn numenter $color5

g512-led -kn ctrl_left $color6
g512-led -kn win_left $color6
g512-led -kn alt_left $color6
g512-led -kn space $color6
g512-led -kn alt_right $color6
g512-led -kn win_right $color6
g512-led -kn menu $color6
g512-led -kn ctrl_right $color6
g512-led -kn arrow_left $color6
g512-led -kn arrow_bottom $color6
g512-led -kn arrow_right $color6
g512-led -kn num0 $color6
g512-led -kn num. $color6

g512-led -c