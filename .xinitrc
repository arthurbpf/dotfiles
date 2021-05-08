#!/bin/sh

session=${1:-xmonad}

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"

fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# Enables numlock (numlockx package needs to be installed)
numlockx &

case $session in
	plasma | kde )
		export DESKTOP_SESSION=plasma
		$HOME/.screenlayout/screenlayout.sh
		exec startplasma-x11
		;;
	xmonad )
		export QT_QPA_PLATFORMTHEME=qt5ct
		$HOME/.screenlayout/screenlayout.sh
		exec xmonad
		;;
	dwm )
		export QT_QPA_PLATFORMTHEME=qt5ct
		$HOME/.screenlayout/screenlayout.sh
		exec dwm
		;;
	# No known session, try to run it as command
	* )
		exec $1
		;;
esac
