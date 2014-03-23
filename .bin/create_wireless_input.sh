#!/bin/bash

# Riitek Microkeyboard is my wireless device
# $ xinput list
#⎡ Virtual core pointer                    	id=2	[master pointer  (3)]
#⎜   ↳ Virtual core XTEST pointer              	id=4	[slave  pointer  (2)]
#⎜   ↳ Symantec Touchpad                	id=12	[slave  pointer  (2)]
#⎜   ↳ Riitek Micro Keyboard                   	id=14	[slave  pointer  (2)]
#⎣ Virtual core keyboard                   	id=3	[master keyboard (2)]
#    ↳ Virtual core XTEST keyboard             	id=5	[slave  keyboard (3)]
#    ↳ AT Translated Set 2 keyboard            	id=11	[slave  keyboard (3)]
#    ↳ Riitek Micro Keyboard                   	id=13	[slave  keyboard (3)]
#
# ----------------
# I run this to create a separate pointer with own keyboard focus for the wireless device:
# $ create_wireless_input.sh up Rii
#Creating xinput master devices prefix=wi...
#xinput --reattach 14 15
#xinput --reattach 13 16
#  

wi_prefix='wi'
wi_pointer="$wi_prefix pointer"
wi_keyboard="$wi_prefix keyboard"

grep_xinput_for() {
   xinput list --short | grep "$@" | sed -e 's/^.*\(id=\([0-9]*\)\).*$/\2/'
}

grep_xinput_twice() {
   xinput list --short | grep "$1" | grep "$2"  | sed -e 's/^.*\(id=\([0-9]*\)\).*$/\2/'
}

create_master() {
    xinput --create-master $wi_prefix     # optional args: <sendCore> <enabled>
}

create_input_for_keyboard_and_mouse() {
    sp=$(grep_xinput_twice "$1" "pointer");
    if [ "$sp" == "" ]; then
        echo "Can't find pointer named $1" >&2
        return 1
    fi

    sk=$(grep_xinput_twice "$1" "keyboard")
    if [ "$sk" == "" ]; then
        echo "Can't find keyboard named $1" >&2
        return 1
    fi

    mp=$(grep_xinput_for "$wi_pointer")
    if [ "$mp" == "" ]; then 
        echo "Creating xinput master devices prefix=$wi_prefix..."
        create_master
        mp=$(grep_xinput_for "$wi_pointer")
    else
        echo "... master pointer $mp already present, assuming keyboard present too"
    fi
    mk=$(grep_xinput_for "$wi_keyboard")
    echo "xinput --reattach $sp $mp"
    xinput --reattach $sp $mp
    echo "xinput --reattach $sk $mk"
    xinput --reattach $sk $mk
}

remove_masters() {
    mp=$(grep_xinput_for "$wi_pointer")
    if [ "$mp" != "" ]; then 
        echo "Removing xinput master pointer"
        xinput --remove-master $mp AttachToMaster
    else
        echo "... master pointer not present"
    fi
    mk=$(grep_xinput_for "$wi_keyboard")
    if [ "$mk" != "" ]; then 
        echo "Removing xinput master keyboard"
        xinput --remove-master $mk AttachToMaster
    else
        echo "... master keyboard not present"
    fi
}

case "$1" in
    'up'|'create')
        shift
        create_input_for_keyboard_and_mouse $@
        ;;
    'down'|'remove')
        shift
        remove_masters
        ;;
    *)
        echo "usage: $0 (<up|create> <device-name-part> | <down|remove>)" >&2
        exit 1
        ;;
esac
