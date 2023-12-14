#!/bin/sh

# Wait for the notification daemon to finish launching
while ! pgrep -f "xmobar" > /dev/null; do

    # Set optional delay
    sleep 0.1
done

while ! pgrep -f "trayer" > /dev/null; do

    # Set optional delay
    sleep 0.1
done

# Play awesome song (do-doop da-doop doop-doop-doop...)
sleep 2
compfy -b
