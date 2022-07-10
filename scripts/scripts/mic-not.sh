#! bin/sh

pactl set-source-mute @DEFAULT_SOURCE@ toggle

# Query amixer for the current volume and whether or not the speaker is muted
mute="$(pactl get-source-mute @DEFAULT_SOURCE@ | awk '{print $2}')"
if [[ "$mute" == "yes" ]]; then
    dunstify -r 191 -a "changeMic" -u low -i audio-volume-muted "Mic muted"
else
    # Show the volume notification

    dunstify -r 191 -a "changeMic" -u low -i audio-volume-muted "Mic unmuted"
fi
