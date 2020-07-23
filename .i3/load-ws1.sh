#!/bin/bash

# First we append the saved layout of worspace N to workspace M
#i3-msg "split h"
sleep 1
i3-msg "workspace $ws1; exec --no-startup-id terminal"
sleep 1
i3-msg "workspace $ws1; exec --no-startup-id terminal"
sleep 1
i3-msg "split v"
i3-msg "workspace $ws1; exec --no-startup-id terminal"
sleep 1
i3-msg "split h"
###sleep 1
#i3-msg "exec --no-startup-id firefox"
#i3-msg "workspace 1:Terminals; "

# And finally we fill the containers with the programs they had
#terminal
#terminal
#terminal 
