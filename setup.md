## Pasos para completar la instalación del sistema

1. Clonar estos dotfiles al `home`:

```bash
git clone https://github.com/luis-barrera/linux-dotfiles.git
```

2. Usar `gnu-stow` para instalar la configuración en el sistema:

```bash
sudo pacman -S stow
```

```bash
cd linux-dotfiles
```

```bash
stow */
cd ..
```

3. Instalar el resto de paquetes:

Con pacman:

```bash
pacman -S --needed - < pacman-packages.txt
```

Con [paru](https://github.com/morganamilo/paru) (esto va tomar mucho tiempo ya que hay paquetes que necesitan ser compilados localmente):

```bash
paru -S --needed - < aur-packages.txt
```

Con python-pip:

```bash
pip install < python-packages.txt
```

4. Config awesome (revisar que cosas como las fuentes, los iconos,los programas, los keybindings y los fondos de pantalla funcionen correctamente) y [ly](https://github.com/nullgemm/ly).
5. Jalar el contenido de pcloud a local.
6. Importar los marcadores de Firefox y Vivaldi (o Chromium).
7. Cambiar de shell a `zsh`:

```bash
chsh -s \bin\zsh
```

8. Verificar que funcione el archivo `.xprofile`.
9. Instalar [oh-my-zsh](https://ohmyz.sh/#install).
10. Cambiar el prompt de `zsh` a [powerlevel10k](https://github.com/romkatv/powerlevel10k#oh-my-zsh).
11. Pasar los alias que están en los dotfiles a `.zshrc`.
12. Configurar (algunos ya tienen configuración en los dotfiles, otros necesitan de configuraciones extras):
	- [haveged](https://wiki.archlinux.org/title/Haveged).
	- [slock](https://wiki.archlinux.org/title/Slock).
	- anki.
	- keepasxc.
	- telegram.
	- neovim.
	- mailspring.
	- spotify y [spicetify](https://github.com/khanhas/spicetify-cli).
	- redshift.
13. Verificar que los temas de gtk3 y de los cursores funcionen correctamente (usar `lxappearance`).
14. Instalar eclipse y descargar soporte para otros lenguajes (C/C++, Java y Java EE).
