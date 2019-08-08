(defun start-manager ()
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
  ;; Load custome packages
										;(add-to-list 'load-path "~/.emacs.d/evil-numbers/")
										;(add-to-list 'load-path "~/.emacs.d/evil")
  (package-initialize))

(defun plugins ()
  (setq evil-want-keybinding nil)

  (require 'evil)
  (require 'evil-numbers)
  (require 'evil-collection)
  (require 'evil-surround)
  (require 'evil-magit)
  (require 'projectile)
  (require 'switch-window)
  (require 'org)
  ;; (require 'evil-org)
  ;; (require 'evil-org-agenda)
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


(defun check-plugins ()
  (setq list-of-plugins '('typescript-mode 'csharp-mode 'projectile 'switch-window 'evil 'org 'helm 'flymd 'doom-themes 'sublime-themes 'php-mode 'key-chord 'google-this 'origami 'autopair 'yasnippet 'web-mode 'web 'golden-ratio 'exec-path-from-shell 'evil-surround 'evil-magit 'evil-collection 'emmet-mode 'company-web 'badwolf-theme))

  (package-refresh-contents)
  (let (value)
	(dolist (element list-of-plugins value)
	  (unless (package-installed-p (cons element value))
		(package-install (cons element value))))))

