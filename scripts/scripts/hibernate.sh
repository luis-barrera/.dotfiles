#!/bin/bash

# luis-barrera 2024

# Script para hibernar a memoria

echo "Hibernar a memoria?"
select yn in "Yes" "No"; do
    case $yn in
        Yes ) systemctl hibernate; break;;
        No ) exit;;
    esac
done
