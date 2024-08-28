#!/bin/bash


pkill swayidle;

swayidle -w timeout 600 'swaylock -f -c 000000' \
            timeout 6000 'systemctl suspend' \
            before-sleep 'swaylock -f -c 000000';
