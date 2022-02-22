#! bin/bash

echo EHC1 > /proc/acpi/wakeup
echo XHCI > /proc/acpi/wakeup
echo IGBE > /proc/acpi/wakeup
echo EXP2 > /proc/acpi/wakeup

cat /proc/acpi/wakeup
