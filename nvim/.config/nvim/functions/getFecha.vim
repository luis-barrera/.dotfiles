" Función que inserta en el documento una linea nueva con la hora y fecha del
" sistema

command! GetFecha .!fecha=`date +"\%a \%d \%b \%Y \%T \%Z"` ; echo "$fecha"
