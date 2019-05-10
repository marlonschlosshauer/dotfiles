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

  ;; Install evil
  (add-to-list 'load-path "~/.emacs.d/evil")
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (require 'evil)
  (when (require 'evil-collection nil t)
	(evil-collection-init))
  (evil-mode 1)

  ;; Require other plugins
  (require 'php-mode)
  (require 'company)
  (require 'company-web-html)
  (require 'undo-tree)
  (require 'goto-chg)
  (require 'evil-surround)
  (require 'evil-magit)
  (require 'yasnippet)
  (require 'autopair)
  (require 'whitespace)
  (require 'key-chord))
