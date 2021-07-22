" Plugin que permite cambiar automáticamente etiquetas de html, si cambios el nombre de una etiqueta abierta, se cambiará también
" en la etiqueta cerrada correspondiente

let g:tagalong_filetypes = ['html', 'xml', 'jsx', 'eruby', 'ejs', 'eco', 'php', 'htmldjango', 'javascriptreact', 'typescriptreact', 'javascript']
" Util para archivos demasido grandes donde no podemos ver que se haya realizado el cambio, nos muestra una salida de texto que
" nos informa si ha hecho algún cambio o no
let g:tagalong_verbose = 1
