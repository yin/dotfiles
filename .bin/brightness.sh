#!/bin/bash

export intel_set=/sys/class/backlight/intel_backlight/brightness
export intel_current=/sys/class/backlight/intel_backlight/actual_brightness
export intel_max=/sys/class/backlight/intel_backlight/max_brightness
export acpi_set=/sys/class/backlight/acpi_video0/brightness
export acpi_current=/sys/class/backlight/acpi_video0/actual_brightness
export acpi_max=/sys/class/backlight/acpi_video0/max_brightness

export ERR_DEVMISSING=100
export ERR_ARGS=1
export ERR_UNSUPPDEV=2
export ERR_WRITE_FAILED=3
export action

if [ $# -eq 0 ]; then
    action=print_usage
fi

while (( "$#" )); do
    case $1 in
        [0-9]*)
            value=$1
            action=${action:-set}
            echo $1 '->' $action
            ;;
        "intel"|"acpi")
            action=${action:-set_$1}
            echo $1 '->' $action
            ;;
        *)
            action=print_usage
            echo $1 '->' $action
            ;;
    esac
    shift
done
echo 'action ->' $action

action_print_current() {
    echo "intel brightness = $(intel_current) (<=$(intel_max))"
    echo "acpi brightness = $(acpi_current) (<=$(acpi_max))"
}

action_set() {
    _action_set intel $@ \
        || _action_set acpi $@ \
        || fail_unsupported_dev
}

action_set_intel() {
    _action_set intel $@ \
        || fail_unsupported_dev
}

action_set_acpi() {
    _action_set acpi $@ \
        || fail_unsupported_dev
}

_action_set() {
    iface=$1
    value=$2
    shift;shift
    eval path=\$${iface}_set
    if [ -e $path ]; then
        echo "setting $iface brightness $(${iface}_current) => $value"
        echo "echo $value | sudo tee $path"
        echo $value | sudo tee $path && \
            return;
        return $ERR_WRITE_FAILED
    fi
    return $ERR_DEVMISSING
}

fail_unsupported_dev() {
    echo "error: unsupported backlight interface" >&2
    exit $ERR_UNSUPPDEV
}

action_print_usage() {
    echo "usage: brightness.sh [acpi|intel] <brightness>" >&2
    action_print_current $@
    exit $ERR_ARGS
}

for i in intel_max acpi_max intel_current acpi_current; do
    eval "$i() { cat \$$i; }"
done

# execute the action
action_$action $value
