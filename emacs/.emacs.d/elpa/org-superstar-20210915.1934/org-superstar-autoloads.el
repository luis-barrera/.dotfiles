;;; org-superstar-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-superstar" "../../../../../.emacs.d/elpa/org-superstar-20210915.1934/org-superstar.el"
;;;;;;  "81c9fb83646017c88f4b46d7790f123e")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/org-superstar-20210915.1934/org-superstar.el

(put 'org-superstar-leading-bullet 'safe-local-variable #'char-or-string-p)

(autoload 'org-superstar-toggle-lightweight-lists "org-superstar" "\
Toggle syntax checking for plain list items.

Disabling syntax checking will cause Org Superstar to display
lines looking like plain lists (for example in code) like plain
lists.  However, this may cause significant speedup for org files
containing several hundred list items." t nil)

(autoload 'org-superstar-mode "org-superstar" "\
Use UTF8 bullets for headlines and plain lists.

If called interactively, enable Org-Superstar mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "org-superstar"
;;;;;;  "../../../../../.emacs.d/elpa/org-superstar-20210915.1934/org-superstar.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../../.emacs.d/elpa/org-superstar-20210915.1934/org-superstar.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-superstar" '("org-superstar-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../../../.emacs.d/elpa/org-superstar-20210915.1934/org-superstar-autoloads.el"
;;;;;;  "../../../../../.emacs.d/elpa/org-superstar-20210915.1934/org-superstar.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-superstar-autoloads.el ends here
