#!/bin/bash

# Run me to apply my modifications to keymappings:
# Menu = M.click, Shift+Menu = R.click 
xmodmap -e 'keycode 135 = Pointer_Button2 Pointer_Button3'

