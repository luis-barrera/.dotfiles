#!/bin/bash

# Script para agregar elementos modificados
# Usa Incron como monitor de eventos en un directorio
# Voy a llamar al script con dos parámetros: el path del archivo y el evento ocurrido
# Esto con el fin de poder detectar archivos temporales.

# Aprovechando que tenemos este script, voy a quitar la parte principal del path
# Así en lugar de poner /home/user/google-drive/archivo solo aparezca ./archivo

# La entrada que recibimos de fswatch es un solo string que contienen
# dos args, tenemos que dividirlo en dos por el espacio entre los dos
# IFS=$'\v' read filepath event <<<"$(echo "$1" | sed 's/./&\v/g')"
read filepath event <<<"$1"

# Archivo en el que vamos a guardar
store_file="/home/luisbarrera/drive-push"

# Eventos aceptados
declare -a events_list=("Created" "Updated" "Removed" "Renamed" "AttributeModified" "MovedFrom" "MovedTo")
declare -A events
for key in "${!events_list[@]}"; do events[${events_list[$key]}]="$key"; done

# Guardar solo eventos aceptados
if [[ -n "${events[$event]}" ]]; then

	# Cambiamos el path a algo más limpio
	new_path=$(sed 's|/home/luisbarrera/google-drive/||g' <<< "$filepath")

	# Ahora nos importa saber si el evento es IN_DELETE y también que el
	# path ya exista en el archivo store bajo el evento Created & Updated
	new_line="$new_path Created"
	new_line2="$new_path Updated"

	if [ $event == "Removed" ] && ( grep -q "$new_line" "$store_file" || grep -q "$new_line2" "$store_file" ); then
		# Como se eliminó un archivo que detectamos como creado anteriormente
		# quire decir que tenemos un archivo temporal que debemos borrar

		# Preprocesamos el path para que podamos pasarlo como argumento
		valid=$(echo $new_line | sed 's_/_\\/_g')
		sed -i "/$valid/d" $store_file

		# Terminamos el programa
		exit 0
	fi

	# Si no se cumplen las condiciones de que es un evento IN_DELETE y además el
	# path ya estaba por un evento IN_CREATE vamos a guardar el path solo si no
	# existe ya en el archivo store
	if grep -q "$new_path" "$store_file"; then
		# Preprocesamos el path para que podamos pasarlo como argumento
		valid=$(echo $new_path | sed 's_/_\\/_g')
		sed -i "/$valid/d" $store_file
	fi

	new_line="$new_path $event"
	echo $new_line >> "$store_file"
fi
