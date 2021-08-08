#!/bin/sh -e
for device in XHC PWRB
do
	if grep -q "$device.*enabled" /proc/acpi/wakeup
	then
		echo $device > /proc/acpi/wakeup
	fi
done
exit 0
