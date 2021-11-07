;; Quitar mensaje de startup
(setq inhibit-startup-message t)

(scroll-bar-mode -1) ;; Desactiva las scrollbars
(tool-bar-mode -1) ;; Desactiva la tool bar
(tooltip-mode -1) ;; Desactiva los tooltips
;;(set-fringe-mode 10) ;; TODO
(menu-bar-mode -1) ;; Desactiva el menubar

;; La pantalla parpadea cuando hay errores
(setq visible-bell t)

;; Establecemos la fuente, para el tamaño multiplicar por 10
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 140)

;; Mostrar en la barra inferior la columna en la que está el cursor
(column-number-mode t)
;; Columna de números
(global-display-line-numbers-mode 1)
;; Numeros de linea relativos
;; BUG: no está funcionando en buffers de org mode, a veces se
;; desactiva en algunos otros
(setq display-line-numbers-type 'relative)
;; Deshabilitar la columna de números en algunos modos
(dolist (mode '(term-mode-hook
 		shell-mode-hook))
   (add-hook mode (lambda () (display-line-numbers-mode 'nil))))
;; Cortar lineas
(global-visual-line-mode t)

;; Abrir ventanas a la derecha y no debajo
(setq split-height-threshold nil)
(setq split-width-threshold 0)
;(setq split-window-right)



;; ##############################
;; Package administrator
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))



;; ##############################
;; Auto-completado
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; Iconos para la barra de estado
;; Después de instalar el paquete, correr M-x all-the-icons-install-fonts
(use-package all-the-icons)

;; Barra de estado inferior
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 12))

;; Temas, desde Doom-Emacs
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Temas favoritos
  ;; Si queremos tener una lista completa de temas usar M-x counsel-load-theme RET
  ;(load-theme 'doom-one t)
  ;(load-theme 'doom-acario-dark t)
  ;(load-theme 'doom-gruvbox t)
  ;(load-theme 'doom-Iosvkem t)
  ;(load-theme 'doom-manegarm t)
  ;(load-theme 'doom-peacock t)
  (load-theme 'doom-tomorrow-night t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Temas, desde base16-emacs
;; (use-package base16-theme
;;   :ensure t
;;   :config
;;   ;; Temas favoritos
;;   ;; (load-theme 'base16-3024 t)
;;   ;; (load-theme 'base16-atelier-cave t)
;;   ;; (load-theme 'base16-atelier-dune t)
;;   ;; (load-theme 'base16-atelier-forest t)
;;   ;; (load-theme 'base16-atelier-heath t)
;;   ;; (load-theme 'base16-atelier-plateau t)
;;   ;; (load-theme 'base16-brewer t)
;;   ;; (load-theme 'base16-chalk t)
;;   ;; (load-theme 'base16-darktooth t)
;;   ;; (load-theme 'base16-gruvbox-dark-hard t)
;;   ;; (load-theme 'base16-irblack t)
;;   ;; (load-theme 'base16-spacemacs t)
;;   ;; (load-theme 'base16-atelier-plateau t)

;;   (setq base16-distinct-fringe-background 'nil
;; 	;; Mode line
;; 	base16-highlight-mode-line 'contrast))

;; Paréntesis de colores
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Which key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; Muestra más info acerca de los comandos, así como su keybinding si está disponible
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; Ayuda
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel.ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

;; Mejora al sistema de ayuda para la documentación de Emacs
(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Leader key con <espacio>, como en vim
(use-package general
  :config
  (general-create-definer rune/leader-keys
			  :keymaps '(normal visual emacs)
			  ;:prefix "SPC"
			  :prefix "C-SPC")

  ;; Estos son ejemplos de cómo se definen los keybindings
  (rune/leader-keys
   "t" '(:ignore t :which-key "toggles")
   "tt" '(counsel-load-theme :which-key "choose theme")))

;; Ayuda a repetir comandos
;; (use-package hydra)



;; ##############################
;; Keybindings
;; Evil mode, capa de vim
(setq evil-want-keybinding nil)
(setq evil-want-integration t)
(unless (package-installed-p 'evil)
  (package-install evil))
(require 'evil)
(evil-mode 1)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(setq evil-ex-search-vim-style-regexp t)
(setq evil-in-single-undo t)

;; Configuración de keybindings de evil para varios modos
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Cambiar entre buffers
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

;; Tener bloques de código sin perder el nivel de jerarquía
; org-mode no deja que metas un subtítulo y luego regreses al título anterior
; cosa que puedes hacer en LaTeX, pero aquí no. Para lograr algo similar está
; este paquete que pertenece a org-mode pero está desactivado por defecto.
; Para usarlo está el key =C-c C-x t=
(require 'org-inlinetask)

;; Incremento/Decremento como en vim
(use-package evil-numbers
  :after evil)
(evil-define-key '(normal visual) 'global (kbd "+") 'evil-numbers/inc-at-pt)
(evil-define-key '(normal visual) 'global (kbd "-") 'evil-numbers/dec-at-pt)

;; Mejor soporte para undo y redo
(use-package undo-fu
  :after evil)
(define-key evil-normal-state-map "u" 'undo-fu-only-undo)
(define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)

;; Usa tecla ESC para cancelar comandos
(define-key isearch-mode-map [escape] 'isearch-abort)
(define-key isearch-mode-map "\e" 'isearch-abort)
(global-set-key [escape] 'keyboard-escape-quit)



;; ##############################
;; Git y manejo de proyectos
;; Manejo de proyectos según le directorio en el que estamos
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ; Si entramos a cualquier directorio dentro de ~/dev se carga una
  ; configuración global dentro de ese directorio
  (when (file-directory-p "~/dev")
  (setq projectile-project-search-path '("~/dev")))
  ; Cuando entramos a un directorio, dired también se mueva a ese
  ; directorio
  (setq projectile-switch-project-action #'projectile-dired))

;; Counsel para projectile
(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; Soporte para git y otros vcs
;; Debemos entrar a magit-status, con s podemos mover archivos al unstaged y con u los sacamos
;; (use-package magit
;;   :custom
;;   (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Soporte de Magit para GitHub y GitLab
;; (use-package forge)



;; ##############################
;; Org mode
(use-package org
  :hook (org-mode . org-indent-mode)
  :config
  ;; Guardar los buffers después de hacer un refile
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  ;; Seguir links al dar RET sobre ellos
  (setq org-return-follows-link t)
  ;; Abrir los links en el mismo frame
  (setq org-link-frame-setup '((file . find-file)))
  ;; Símbolo a usar cuando cuando hay texto oculto por usar S-TAB
  (setq org-ellipsis "⥥")
  ;; Elimina los símbolos que se usan para modificar los caracteres,
  ;; por ejemplo: los * que se usan para hacer negritas
  (setq org-hide-emphasis-markers t)
  ;; Org agenda
  ;; Guardar un log de las tareas completas
  (setq org-agenda-start-with-log-mode t)
  ;; Guardar la fecha cuando marcamos algo como completo
  (setq org-log-donde 'time)
  ;; Mantener el log dentro de un drawer, de manerar que se hace un fold 
  (setq org-log-into-drawer t)
  ;; Archivos considerados por org-agenda
  (setq org-agenda-files '("~/org-mode/Tareas21O.org" "~/org-mode/Clases21O.org"))
  ;; (setq org-agenda-files '("~/org-mode/tasks.org" "~/org-mode/clases21O.org"))
  ;; Mostrar 10 días en la vista de semana de org-agenda.
  (setq org-agenda-span 10)
  ;; Keywords para tasks
  (setq org-todo-keywords '((sequence "TODO" "DOING" "|" "DONE"))))

;; Seguir links de org mode al dar RET sobre ellos
(setq org-return-follows-link t)
;; Maping para abrir la agenda
(global-set-key (kbd "C-c a") 'org-agenda-list)

;; Cambiar los símbolos de los headings
(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  ; Caracteres a usar para cada nivel
  (setq org-superstar-headline-bullets-list '("❉" "❋" "✺" "✹" "✸" "✶" "✵" "✲"))
  ; Al llegar más allá de 8 niveles, nil=usar siempre el último caracter, t=ciclar entre los elementos
  (setq org-superstar-cycle-headline-bullets 'nil)
  ; Ocultar un puntito que sale enfrente de los heading
  (setq org-hide-leading-stars nil)
  (setq org-superstar-leading-bullet ?\s)
  (setq org-indent-mode-turns-on-hiding-stars nil))

;; Zettlekasten en org-mode
(use-package org-roam
  :ensure t
  :init (setq org-roam-v2-ack t)
  :custom
  (setq org-roam-directory "~/org-roam")
  ;(setq org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ; Esta es la función con la que debemos empezar
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-M-i" . completion-at-point))
  :config
  ;(org-roam-setup)
  (setq org-roam-completion-everywhere t))

;; Templates para org-roam
(setq org-roam-capture-templates
      '(("d" "default" plain "%?"
	 :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			    "#+title: ${title}\n")
	 :unnarrowed t)
	("c" "Nota completa" plain "%?"
	 :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			    ":PROPERTIES:
:AUTHOR: %^{Author|No Author}
:ROAM_REFS: %^{Link o Ref|No Ref}
:ROAM_ALIAS: %^{Alias|No Alias}
:TYPE: %^{Type|Articulo|Noticia|Libro|Video}
:DATE: %T
:CREATOR: luis-barrera
:END:
#+title: ${title}
#+filetags:
")
	 :unnarrowed t)))

;; Pegar imágenes en las notas desde el clipboard o abre una app para hacer screenshot
(use-package org-download
  :after org
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank))))

;; Navegar entre ventanas de mejor manera usando winner mode
(winner-mode +1)
(define-key winner-mode-map (kbd "<M-left>") #'winner-undo)
(define-key winner-mode-map (kbd "<M-right>") #'winner-redo)

;; Evil para org, hay algunas teclas que no funcionan correctamente en org mode debido a evil
(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Poner bordes a los lados del editor, solo en org-mode
(use-package visual-fill-column
  :defer t
  :hook (org-mode . efs/org-mode-visual-fill))

;; Poner bordes a los lados del editor, solo en org-mode
(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

;; Quitar los keybindings de RET y SPC
(defun my-move-key (keymap-from keymap-to key)
     (define-key keymap-to key (lookup-key keymap-from key))
     (define-key keymap-from key nil))
(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(display-line-numbers 'relative)
 '(package-selected-packages
   '(base16-theme org-download undo-fu evil-numbers org-superstar visual-fill-column forge magit counsel-projectile projectile evil-collection general all-the-icons-completion treemacs-all-the-icons doom-themes helpful counsel ivy-rich which-key rainbow-delimiters org-evil command-log-mode use-package))
 '(safe-local-variable-values
   '((eval setq org-image-actual-width 200)
     (eval org-display-inline-images t t)
     (eval setq org-image-actual-width 100))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :height 1.5))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.3))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.2))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.1)))))

;; Poner los archivos de Backup (los que terminan en ~) en otro lugar
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

;; Transparencia en el fondo
(defun toggle-transparency ()
   (interactive)
   (let ((alpha (frame-parameter nil 'alpha)))
     (set-frame-parameter
      nil 'alpha
      (if (eql (cond ((numberp alpha) alpha)
                     ((numberp (cdr alpha)) (cdr alpha))
                     ;; Also handle undocumented (<active> <inactive>) form.
                     ((numberp (cadr alpha)) (cadr alpha)))
               100)
          '(85 . 50) '(100 . 100)))))
 (global-set-key (kbd "C-c t") 'toggle-transparency)

;; Guardar los buffers abiertos antes de cerrar el editor
(desktop-save-mode 1)
