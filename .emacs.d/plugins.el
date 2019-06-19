(defun plugins ()
  ;; Install package manager
  (require 'package)
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
					  (not (gnutls-available-p))))
		 (proto (if no-ssl "http" "https")))
	;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
	(add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
	;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
	(when (< emacs-major-version 24)
	  ;; For important compatibility libraries like cl-lib
	  (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
  (package-initialize)


  ;; Load custome packages
  ;(add-to-list 'load-path "~/.emacs.d/evil-numbers/")
  ;(add-to-list 'load-path "~/.emacs.d/evil")

  (setq evil-want-keybinding nil)

	(require 'evil)
	(require 'evil-numbers)
	(require 'evil-collection)
	(require 'evil-surround)
	(require 'evil-magit)
	(require 'projectile)
	(require 'switch-window)
	(require 'org)
	(require 'helm-config)
	(require 'php-mode)
	(require 'company)
	(require 'company-web-html)
	(require 'undo-tree)
	(require 'goto-chg)
	(require 'yasnippet)
	(require 'autopair)
	(require 'whitespace)
	(require 'key-chord)
	(require 'tramp))
