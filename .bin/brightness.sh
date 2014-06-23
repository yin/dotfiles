#!/bin/bash

intel=/sys/class/backlight/intel_backlight/brightness
intel_current=/sys/class/backlight/intel_backlight/actual_brightness
intel_max=/sys/class/backlight/intel_backlight/max_brightness
acpi=/sys/class/backlight/acpi_video0/brightness
acpi_max=/sys/class/backlight/acpi_video0/max_brightness

intel_max() {
    echo "for intel blacklight maximum brightness is $(cat $intel_max)" >&2
}
acpi_max() {
    echo "for acpi video maximum brightness is $(cat $acpi_max)" >&2
}

if [ $# -ne 1 ]; then
    echo "usage: brightness <brightness>" >&2
    intel_max
    acpi_max
    exit 1
fi

if [ -e $intel ]; then
    echo "current intel brightness was $(cat $intel_current)"
    echo -n "Setting intel backlight brightness "
    echo $1 | sudo tee $intel || intel_max
elif [ -e $acpi ]; then 
    echo -n "Setting intel backlight brightness "
    echo $1 | sudo tee $acpi || acpi_max
else
    echo "unsupported backlight interface, can not set backlight brightness" >&2
    exit 2
fi
