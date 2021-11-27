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
;; (set-face-attribute 'default t :font "JetBrainsMono Nerd Font" :height 200)
;; (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font-20")
;; (set-frame-font "JetBrainsMono Nerd Font-20" nil t)
(set-face-attribute 'default nil :font "Iosevka-18")
(set-frame-font "Iosevka-18" nil t)

;; Indentación
;; Se recomienda usar también los comando tabify y untabify
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Cortar lineas
(global-visual-line-mode t)
;; Mostrar en la barra inferior la columna en la que está el cursor
(column-number-mode t)
;; Columna de números
;; (global-display-line-numbers-mode 1)
(defun my-display-numbers-hook ()
  (display-line-numbers-mode t)
  (setq display-line-numbers-type 'relative)
)
(add-hook 'prog-mode-hook 'my-display-numbers-hook)
(add-hook 'text-mode-hook (display-line-numbers-mode 'nil))
(add-hook 'org-mode-hook (display-line-numbers-mode 'nil))
;; Numeros de linea relativos
;; BUG: no está funcionando en buffers de org mode, a veces se
;; desactiva en algunos otros
;; (setq display-line-numbers-type 'relative)
;; Deshabilitar la columna de números en algunos modos
;; (dolist (mode '(term-mode-hook
;;  		shell-mode-hook))
;;    (add-hook mode (lambda () (display-line-numbers-mode 'nil))))

;; Abrir ventanas a la derecha y no debajo
(setq split-height-threshold nil)
(setq split-width-threshold 0)
;(setq split-window-right)



;; ##############################
;; Package administrator
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))



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
  :custom (doom-modeline-height 10))
(setq doom-modeline-buffer-file-name-style 'truncate-upto-root)

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
  (load-theme 'doom-acario-dark t)
  ;(load-theme 'doom-gruvbox t)
  ;(load-theme 'doom-Iosvkem t)
  ;(load-theme 'doom-manegarm t)
  ;(load-theme 'doom-peacock t)
  ;(load-theme 'doom-tomorrow-night t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

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
;; (use-package general
;;   :config
;;   (general-create-definer rune/leader-keys
;; 			  :keymaps '(normal visual emacs)
;; 			  ;:prefix "SPC"
;; 			  :prefix "C-SPC")

;;   ;; Estos son ejemplos de cómo se definen los keybindings
;;   (rune/leader-keys
;;    "t" '(:ignore t :which-key "toggles")
;;    "tt" '(counsel-load-theme :which-key "choose theme")))

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
  ;; Deja de insertar indentación en los bloques de código
  (setq org-edit-src-content-indentation 0)
  ;; Seguir links al dar RET sobre ellos
  (setq org-return-follows-link t)
  ;; Abrir los links en el mismo frame
  (setq org-link-frame-setup '((file . find-file)))
  ;; Símbolo a usar cuando cuando hay texto oculto por usar S-TAB
  (setq org-ellipsis "⥥")
  ;; Elimina los símbolos que se usan para modificar los caracteres,
  ;; por ejemplo: los * que se usan para hacer negritas
  (setq org-hide-emphasis-markers nil)
  ;; Org agenda
  ;; Guardar un log de las tareas completas
  (setq org-agenda-start-with-log-mode t)
  ;; Guardar la fecha cuando marcamos algo como completo
  (setq org-log-done 'time)
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
  (setq org-superstar-headline-bullets-list '("𝍠" "𝍡" "𝍢" "𝍣" "𝍤" "𝍥" "𝍦" "𝍧"))
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

;; Encontrar palabras dentro de los archivos
(use-package deft
    :config
    (setq deft-directory org-roam-directory
          deft-recursive t
          deft-use-filename-as-title t)
    :bind
    ("C-c n d" . deft))

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



;; TODO: partes desordenadas, funcionan pero ponerlas en su lugar correspondiente
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
 ;; '(display-line-numbers 'relative)
 '(package-selected-packages
   '(solaire-mode neotree lean-mode js-react-redux-yasnippets yasnippet-classic-snippets yasnippet-lean company-try-hard php-mode emmet-mode yasnippet-snippets company-web web-mode sequential-command alert pomm deft org-download undo-fu evil-numbers org-superstar visual-fill-column forge magit counsel-projectile projectile evil-collection general all-the-icons-completion treemacs-all-the-icons doom-themes helpful counsel ivy-rich which-key rainbow-delimiters org-evil command-log-mode use-package))
 '(safe-local-variable-values
   '((eval setq org-image-actual-width 200)
     (eval org-display-inline-images t t)
     (eval setq org-image-actual-width 100)))
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:family "Iosevka" :height 0.8))))
 '(mode-line-inactive ((t (:family "Iosevka" :height 0.8))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.25))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.15))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.05))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))

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
          '(90 . 50) '(100 . 100)))))
 (global-set-key (kbd "C-c t") 'toggle-transparency)

;; Guardar los buffers abiertos antes de cerrar el editor
(desktop-save-mode 1)


;; Notificaciones de escritorio para los eventos de org-agenda, por
;; defecto Emacs no puede enviar notificaciones al escritorio
(require 'appt)
(appt-activate t)

(setq appt-message-warning-time 5) ; Show notification 5 minutes before event
(setq appt-display-interval appt-message-warning-time) ; Disable multiple reminders
(setq appt-display-mode-line nil)

; Use appointment data from org-mode
(defun my-org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

; Update alarms when...
; (1) ... Starting Emacs
(my-org-agenda-to-appt)
; (2) ... Everyday at 12:05am (useful in case you keep Emacs always on)
(run-at-time "12:05am" (* 24 3600) 'my-org-agenda-to-appt)
(run-at-time "07:35am" (* 24 3600) 'my-org-agenda-to-appt)
; (3) ... When TODO.txt is saved
(add-hook 'after-save-hook
          '(lambda ()
             (if (string= (buffer-file-name) (concat (getenv "HOME") "/org-mode/Tareas21O.org"))
                 (my-org-agenda-to-appt))))
(add-hook 'after-save-hook
          '(lambda ()
             (if (string= (buffer-file-name) (concat (getenv "HOME") "/org-mode/Clases21O.org"))
                 (my-org-agenda-to-appt))))

; Display appointments as a window manager notification
(setq appt-disp-window-function 'my-appt-display)
(setq appt-delete-window-function (lambda () t))

(setq my-appt-notification-app (concat (getenv "HOME") "/bin/appt-notification"))

(defun my-appt-display (min-to-app new-time msg)
  (if (atom min-to-app)
    (start-process "my-appt-notification-app" nil my-appt-notification-app min-to-app msg)
  (dolist (i (number-sequence 0 (1- (length min-to-app))))
    (start-process "my-appt-notification-app" nil my-appt-notification-app (nth i min-to-app) (nth i msg)))))

;; Notificaciones en el escritorio
(setq alert-default-style 'libnotify)



;; ##############################
;; Web-mode
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))

;; Resalta las columnas
(setq web-mode-enable-current-column-highlight t)
;; (setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-element-highlight 'nil)

;; Autocompletado en línea
(defun my-web-mode-hook ()
  (set (make-local-variable 'company-backends) '(company-css company-web-html company-yasnippet company-files))
)

;; Activar emmet en web-mode
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'web-mode-hook  'emmet-mode) 
;; Editar js, css en html-mode
(add-hook 'web-mode-before-auto-complete-hooks
    '(lambda ()
     (let ((web-mode-cur-language
  	    (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "php")
    	   (yas-activate-extra-mode 'php-mode)
      	 (yas-deactivate-extra-mode 'php-mode))
               (if (string= web-mode-cur-language "css")
    	   (setq emmet-use-css-transform t)
      	 (setq emmet-use-css-transform nil)))))
;; Emmet usa por defecto dos tabs(o espacios) para indentar, usar solo 1 tab
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert t)))
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 1)))
;; Mover el cursor entre quotes
(setq emmet-move-cursor-between-quotes t) ;; default nil

;; Hora en la modeline
(display-time-mode 1)

;; Mover la indentación por bloques
(global-set-key (kbd "C->") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "C-<") 'indent-rigidly-left-to-tab-stop)

;; Autocompletado con company
;; Funciona a través de backends, para una lista completa de los que
;; están habilitados usar el comando =M-x customize-variable RET
;; company-backends=.
(add-hook 'after-init-hook 'global-company-mode)

;; Snippets, autocompletado
;; (yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Autopair paréntesis
(electric-pair-mode)

;; Neotree, file manager
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(defun neotree-setup ()(interactive)(progn(text-scale-adjust 0)(text-scale-decrease 0.4)))
(add-hook 'neo-after-create-hook
          (lambda (_)(call-interactively 'neotree-setup))
          (global-visual-line-mode 'nil)
          (display-line-numbers-mode 'nil))

;; Quita el path completo para symbolik links
(setq find-file-visit-truename t)

;; Solaire-mode
(solaire-global-mode +1)
