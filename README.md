# Dofiles de mi sistema Linux. Usando un bare repository.

Un bare repo, es un repositorio de git que no es el directorio actual de trabajo de un proyecto dado.

Se trata de un directorio en el vamos a guardar elementos de otros directorios de diferentes lugares del
sistema para que sean almacenados en un solo repositorio de git.

No es necesario guardar información en este directorio, solo debemos hacer link a los archivos que queremos
agregar al repositorio.

Hay un alias en el .zshrc llamado dotfiles que permite manipular las acciones de git de este repositorio



Con `dotfiles add .zshrc` para agregar un archivo al repositorio.

Con `dotfiles commit -m "Algún comentario"` para realizar un commit.

En sí, cualquier comando de git relacionado a este repositorio se debe ejecutar con `dotfiles` en lugar
de `git`.
