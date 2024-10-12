# Dos instancias separadas de navegador

Para poder tener dos navegadores abiertos al mismo tiempo debemos configurar
las entradas de escritorio.

En cada una de esas entradas, tenemos un perfil por separado. `chromium` (y los
navegadores basados en el) tiene implementado un singleton que no permite tener
abiertas dos sesiones del proceso del navegador al mismo tiempo ([Starting 2
instances of Chrome in separate tabs
(Linux)](https://groups.google.com/a/chromium.org/g/chromium-dev/c/kqdEe0I9DQw?pli=1)).

Una entrada de escritorio son archivos con terminación `.desktop` que se
encuentra comunmente en el directorio `/usr/share/applications`. Indican las
propiedades que el Desktop Environment debe usar para abrir un programa desde
el menú de aplicaciones que este tiene u otro launcher como `rofi`. Nos vamos a
enfocar en los valores de `StartupWMClass`, `Exec`, y `Name`.

El navegador usa un carpeta para guardar la información que requiere para
funcionar: bookmarks, pestañas abiertas, historial, plugins. étc.

Se debe poner en una entrada de escritorio, en la parte `Exec`, la opción de
`--user-data-dir` indicando a una carpeta diferente a la que se usa por
defecto. Para saber dónde está esa carpera debemos entrar a la siguiente
dirección desde el navegador `brave://version/` y en `Command Line` indica el
comando que contiene la parte de `--user-data-dir`. Vamos a ese directorio, y
lo copiamos en otro lugar, para cambiarlo en uno de las entradas de escritorio.

Por último, debemos agregar un flag adiciona al comando. En ambas entradas
debemos cambiar la propiedad `Exec` y agregar `--class`, y el valor debe ser
diferente en ambos, para que el Desktop Environment no se confunda en detectar
las ventanas. El valor que se acaba de poner a la propiedad *class* también lo
vamos a poner en `StartupWMClass`.

Con esto, el launcher y el Desktop Environment pueden lanzar e identificar dos
instancias totalmente separadas del navegador.

En conjunto con una configuración de KDE Plasma, ahora se puede abrir siempre
un navegador en un Virtual Desktop y otro navegador en otro.

[gnome shell - How can I start a separate instance (not profile) of Chromium,
&#x2026;](https://askubuntu.com/questions/1112571/how-can-i-start-a-separate-instance-not-profile-of-chromium-with-its-own-icon)

