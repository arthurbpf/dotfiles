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

# Solves keyring problems
/etc/X11/xinit/xinitrc.d/50-systemd-user.sh &

~/.screenlayout/1.sh

case $session in
	xmonad )
		exec xmonad
		;;
	dwm )
		exec dwm
		;;
	# No known session, try to run it as command
	* )
		exec $1
		;;
esac
