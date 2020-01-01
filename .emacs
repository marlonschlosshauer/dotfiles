;; Lazy fix for #34341 Bad Request
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(require 'package)
(add-to-list 'package-archives
			 '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package dired
  :config
  (setq dired-listing-switches "-alh"))

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-c g") 'magit)
  (setq magit-display-buffer-function
		(lambda (buffer)
		  (display-buffer
		   buffer (if (and (derived-mode-p 'magit-mode)
						   (memq (with-current-buffer buffer major-mode)
								 '(magit-process-mode
								   magit-revision-mode
								   magit-diff-mode
								   magit-stash-mode
								   magit-status-mode)))
					  nil
					'(display-buffer-same-window))))))

(use-package evil
  :ensure t
  :defer .1
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (define-key evil-normal-state-map (kbd "B") 'evil-beginning-of-visual-line)
  (define-key evil-normal-state-map (kbd "E") 'evil-end-of-visual-line)
  (evil-mode 1)

  (use-package evil-magit
	:ensure t)

  (use-package evil-org
	:ensure t
	:after org
	:config
	(add-hook 'org-mode-hook 'evil-org-mode)
	(add-hook 'evil-org-mode-hook
			  (lambda ()
				(evil-org-set-key-theme)))
	(require 'evil-org-agenda)
	(evil-org-agenda-set-keys))

  (use-package evil-collection
	:ensure t
	:after (evil dired)
	:config
	(evil-collection-init)
	(evil-collection-define-key 'normal 'dired-mode-map
	  (kbd ".") 'dired-up-directory))

  (use-package evil-numbers
	:ensure t
	:after evil
	:config
	(define-key evil-normal-state-map (kbd "+") 'evil-numbers/inc-at-pt)
	(define-key evil-normal-state-map (kbd "-") 'evil-numbers/dec-at-pt))

  (use-package evil-multiedit
	:ensure t
	:after evil
	:config
	(evil-multiedit-default-keybinds))

  (use-package evil-surround
	:ensure t
	:after evil
	:config
	(global-evil-surround-mode 1))

  (use-package key-chord
	:ensure t
	:after evil
	:config
	(key-chord-mode 1)
	(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)))

(use-package org
  :mode (("\\.org\\'" . org-mode)
		 ("\\.md\\'" . org-mode))
  :defer t
  :config
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  (setq org-return-follows-link t)
  (setq org-src-tab-acts-natively t)

  ;; Force org to open files in dired, instead of finder
  (add-to-list 'org-file-apps '(directory . emacs))

  (setq org-agenda-files '("~/Dropbox/org/"))
  (setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE" "CANCEL")))
  (setq org-default-notes-file "~/Dropbox/org/todo.org")

  (setq org-agenda-custom-commands
		'(("c" "Simple agenda view"
		   ((agenda "")
			(alltodo "")))))

  (use-package org-pdfview
	:ensure t
	:config
	;;(pdf-tools-install)
	(pdf-loader-install)
	(add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))))

(use-package helm
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'helm-M-x))

(use-package elfeed
  :defer t
  :after evil-collection
  :config
  (add-hook 'elfeed-search-mode-hook
			(lambda ()
			  (evil-collection-define-key 'normal 'elfeed-search-mode-map
				(kbd "RET") 'elfeed-search-browse-url)))
  (setq-default elfeed-search-filter "@2-weeks-ago ")

  (use-package elfeed-org
	:ensure t
	:after org
	:config
	(elfeed-org)
	(setq rmh-elfeed-org-files (list "~/Dropbox/org/rss.org"))))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package python
  :defer t
  :config
  (setq python-shell-interpreter "/usr/local/bin/python3")

  (use-package pyvenv
	:ensure t)

  (use-package flycheck-pyflakes
	:ensure t
	:after flycheck))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :custom
  (lsp-prefer-flymake nil)
  :hook
  ((python-mode) . lsp))

(use-package company
  :ensure t
  :config
  (global-company-mode 1)

  (use-package company-lsp
	:ensure t
	:config
	(push 'company-lsp company-backends)))

(use-package switch-window
  :ensure t
  :config
  (global-set-key (kbd "C-x o") 'switch-window))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  ;; Load this to fix : Symbol’s function definition is void: yasnippet-snippets--fixed-indent
  ;; Caused in python-mode by python-send-buffer
  (load "~/.emacs.d/elisp/experimental.el"))

(use-package autopair
  :ensure t
  :config
  (autopair-global-mode))

(use-package origami
  :ensure t)

(use-package web-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))

  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t)

  (add-hook 'web-mode-hook 'origami-mode)

  (use-package emmet-mode
	:ensure t
	:config
	(add-hook 'web-mode-hook  'emmet-mode)))

(use-package sql
  :defer t
  :config
  (add-hook 'sql-interactive-mode-hook
			(lambda ()
			  (toggle-truncate-lines t))))

(use-package google-this
  :ensure t
  :config
  (google-this-mode 1))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
	(exec-path-from-shell-initialize)))

(use-package restclient
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode)))

(use-package rainbow-mode
  :ensure t
  :config
  (rainbow-mode))

(use-package spotify
  :ensure t
  :bind (("C-c s ." . spotify-playpause)
		 ("C-c s l" . spotify-next)
		 ("C-c s h" . spotify-previous))
  :config
  (setq spotify-oauth2-client-id "")
  (setq spotify-oauth2-client-secret ""))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))
;;; End of packages

;;; Misc
;; Deleted repeating blank lines
(add-hook 'before-save-hook 'whitespace-cleanup)
;;; End of Misc

;;; GUI
;; Removing GUI items
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;;; End of GUI

;; Remove splash screen
(setq inhibit-splash-screen t)

;; Hide macOS top bar
(setq ns-auto-hide-menu-bar t)

;; line numbers
(setq linum-format "%4d  ")
(global-linum-mode t)

;; Highlight closing tags (parenthesis, brackets)
(show-paren-mode t)
(setq show-paren-delay 0)

;; Make top bar light
(add-to-list 'default-frame-alist '(ns-appearance . light))

;;; Built-in Emacs
;; Set enviroment variables
(setenv "PATH"
		(concat
		 "/usr/local/bin" ";"
		 "/usr/bin" ";"
		 "/bin" ";"
		 "/usr/sbin" ";"
		 "/sbin" ";"
		 (getenv "PATH")
		 )
		)

;; Change font color for keywords
(if (fboundp 'global-font-lock-mode)
	(global-font-lock-mode 1))

;; Turn auto reload of buffer (on file change) on
(global-auto-revert-mode t)

;; Enable line wrapping
(add-hook 'text-mode-hook 'auto-fill-mode)

;; Set line wrap
(setq-default fill-column 80)

;; Turn off creation of temp files
(setq make-backup-files nil)
(setq auto-save-default nil)

(add-to-list 'display-buffer-alist
			 `(,(regexp-quote "*shell") display-buffer-same-window))

;; Setup indentation
(setq indent-tabs-mode t)
(setq-default tab-width 4)
(setq c-default-style "linux")
(setq-local c-basic-offset 4)
(setq-local javascript-indent-level 4)
(setq-local js-indent-level 4)
(setq-local js4-basic-offset 4)
(setq-local web-mode-markup-indent-offset 4)
(setq-local web-mode-css-indent-offset 4)
(setq-local web-mode-code-indent-offset 4)
(setq-local css-indent-offset 4)

;; Show tabs
(setq whitespace-style '(face tabs tab-mark trailing))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-tab ((t (:foreground "#636363")))))

(setq whitespace-display-mappings
	  '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'

(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
;;; End of Built-in Emacs

;;; Binds
;; German keyboard binding
(global-set-key (kbd "M-5") '(lambda () (interactive) (insert "[")))
(global-set-key (kbd "M-6") '(lambda () (interactive) (insert "]")))
(global-set-key (kbd "M-7") '(lambda () (interactive) (insert "|")))
(global-set-key (kbd "M-/") '(lambda () (interactive) (insert "\\")))
(global-set-key (kbd "M-8") '(lambda () (interactive) (insert "{")))
(global-set-key (kbd "M-9") '(lambda () (interactive) (insert "}")))
(global-set-key (kbd "M-l") '(lambda () (interactive) (insert "@")))
(global-set-key (kbd "M-n") '(lambda () (interactive) (insert "~")))

(global-set-key (kbd "C-x j") 'xref-find-definitions)
(global-set-key (kbd "C-x p") 'xref-pop-marker-stack)
(global-set-key (kbd "C-x SPC") 'company-complete)
(global-set-key (kbd "C-x v") 'comment-region)
(global-set-key (kbd "C-x r") 'lsp-rename)
(global-set-key (kbd "C-x t") 'lsp-format-buffer)

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

(global-set-key (kbd "C-x C-b") 'ibuffer)
;;; End of Binds
