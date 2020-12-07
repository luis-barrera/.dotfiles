# Dofiles de mi sistema Linux. Usando un bare repository.

Un bare repository, es un repositorio que no es el directorio de trabajo de nuestro repositorio.

Se trata de un directorio en el vamos a guardar elementos de otros directorios para que sean almacenados en un
solo repositorio de git.

No es necesario guardar información en este directorio, solo debemos hacer link a los archivos que queremos
agregar al repositorio.

Hay un alias en el .zshrc llamado dotfiles que permite manipular las acciones de git de este repositorio



Puedo hacer `dotfiles add .zshrc` para agregar un archivo al repositorio.

Y también puedo hacer `dotfiles commit -m "Algún comentario"` para realizar un commit.
