(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

;; Fix emacs looking for incorrect melpa certifications
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Load packages
(package-initialize)

;; Set default dirs
(setq default-directory "~/")
(setq custom-file (concat user-emacs-directory "/custom.el"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

(use-package tree-sitter
  :config (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package exec-path-from-shell
  :init (setenv "SHELL" "/usr/local/bin/zsh")
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package dired
  :config
  (setq dired-listing-switches "-alh"))

(use-package magit
  :bind (("C-c g" . magit)))

(use-package forge
  :after magit)

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package evil
  :after undo-tree
  :defer .1
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :bind (("C-l" . nil) ; Unset non-prefix
	 ("C-l k" . comment-or-uncomment-region))
  :config
  (setq evil-undo-system 'undo-tree)
  (evil-mode 1))

(use-package evil-org
  :after (evil org)
  :config (add-hook 'org-mode-hook 'evil-org-mode))

(use-package evil-org-agenda
  :ensure nil
  :after evil-org
  :config (evil-org-agenda-set-keys))

(use-package evil-collection
  :after (evil dired)
  :config
  (evil-collection-init)
  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd ".") 'dired-up-directory))

(use-package evil-numbers
  :after evil
  :config
  (define-key evil-normal-state-map (kbd "+") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "-") 'evil-numbers/dec-at-pt))

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

(use-package evil-matchit
  :after evil
  :hook ((js-mode . evil-matchit-mode)
	 (typescript-mode . evil-matchit-mode)
	 (web-mode . evil-matchit-mode)))

(use-package key-chord
  :after evil
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state))

(use-package elec-pair
  :config
  (setq electric-pair-pairs
	'((?\" . ?\")
	  (?\' . ?\')
	  (?\( . ?\))
	  (?\[ . ?\])
	  (?\< . ?\>)
	  (?\{ . ?\})))
  (electric-pair-mode 1))

(use-package ace-jump-mode
  :after evil
  :config
  (define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode))

(use-package org
  :pin gnu
  :mode (("\\.org\\'" . org-mode))
  :bind (("C-c a" . org-agenda)
	 ("C-c c" . org-capture)
	 ("C-c l" . org-store-link))
  :config
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  (setq org-return-follows-link t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-preserve-indentation nil)

  ;; Enable exporting of highlighted syntax with minting
  (add-to-list 'org-latex-packages-alist '("" "minted"))

  ;; Add minting option
  (setq org-latex-listings 'minted)
  (setq org-latex-minted-options
	'(("breaklines=true")))

  (setq bibtex-dialect 'biblatex)

  ;; This works with org-ref for some reason
  (setq org-latex-pdf-process '("latexmk -pdflatex='%latex -shell-escape -interaction nonstopmode' -pdf -output-directory=%o -f %f"))

  ;; Make fonts in src buffer look pretty
  (setq org-src-fontify-natively t)

  (setq org-latex-tables-centered nil
	org-tags-column 80)

  ;; Remove line-numbers in agenda
  (add-hook 'org-agenda-mode-hook (lambda() (linum-mode -1)))

  ;; Setup babel support
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (shell . t)
     (python . t)))

  ;; Force org to open files in dired, instead of finder
  (add-to-list 'org-file-apps '(directory . emacs)))

(use-package org-pdfview
  :disabled
  :ensure nil
  :after pdf-tools)

(use-package pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1))))

(use-package elfeed
  :commands elfeed
  :after (elfeed-org evil-collection)
  :config
  (add-hook 'elfeed-search-mode-hook
	    (lambda ()
	      (evil-collection-define-key 'normal 'elfeed-search-mode-map
		(kbd "RET") 'elfeed-search-browse-url)))
  (setq-default elfeed-search-filter "@2-weeks-ago "))

(use-package elfeed-org
  :after (elfeed org)
  :config
  (setq rmh-elfeed-org-files (list "~/Dropbox/org/rss.org"))
  (elfeed-org))

(use-package company
  :bind ("TAB" . company-complete)
  :config
  (setq company-require-match nil)
  (setq company-show-numbers t)
  (setq company-selection-wrap-around t)
  (global-company-mode 1))

(use-package company-restclient
  :defer t
  :after company)

(use-package company-web
  :defer t
  :after (web-mode company))

(use-package lsp-mode
  :commands lsp
  :after (evil)
  :init (setq lsp-keymap-prefix "C-l")
  :hook ((python-mode . lsp)
	 (java-mode . lsp)
	 (css-mode . lsp)
	 (web-mode . lsp)
	 (js-mode . lsp)
	 (typescript-mode . lsp)
	 (haskell-mode . lsp))
  :config
  (define-key evil-normal-state-map (kbd "g t") 'lsp-goto-type-definition)

  ;; Poor performance with languages that don't provide formatter and have formatter setup (py-yapf)
  (add-hook 'before-save-hook 'lsp-format-buffer)
  (setq lsp-headerline-breadcrumb-enable nil
	lsp-diagnostics-provider :flycheck
	lsp-prefer-flymake nil
	lsp-log-io nil))

(use-package lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-doc-enable t
	lsp-ui-peek-enable t
	lsp-ui-sideline-enable nil
	lsp-ui-doc-include-signature nil
	lsp-ui-doc-position 'at-point
	lsp-ui-doc-show-with-cursor t))

(use-package lsp-java
  :defer t
  :after (java-mode lsp))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package clojurescript-mode
  :ensure nil
  :defer t
  :after lsp-mode
  :mode (("\\.cljs?\\'" . clojurescript-mode)))

(use-package clojure-mode
  :after lsp-mode
  :mode (("\\.clj?\\'" . clojure-mode)))

(use-package evil-cleverparens
  :after evil
  :hook ((clojure-mode . evil-cleverparens-mode)
	 (clojurescript-mode . evil-cleverparens-mode)))

(use-package cider
  :bind ("C-l" . cider-repl-clear-buffer)
  :config
  (setq cider-show-error-buffer 'only-in-repl))

(use-package objc-mode
  :ensure nil
  :mode (("\\.mmm?\\'" . objc-mode)
	 ("\\.m?\\'" . objc-mode)))

(use-package groovy-mode
  :mode (("\\.gradle?\\'" . groovy-mode)))

(use-package css-mode
  :mode (("\\.css\\'" . css-mode)))

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
	 ("\\.php\\'" . web-mode))

  :config
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t)

  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package js-mode
  :ensure nil
  :mode (("\\.js?\\'" . js-mode)
	 ("\\.jsx?\\'" . js-mode))
  :config
  (setq javascript-indent-level 1)
  (setq js-indent-level 1))

(use-package typescript-mode
  :mode (("\\.ts?\\'" . typescript-mode)
	 ("\\.tsx?\\'" . typescript-mode))
  :config
  (setq typescript-indent-level 2)
  (setq typescript-auto-indent-flag t))

(use-package emmet-mode
  :config
  :hook ((web-mode . emmet-mode)
	 (typescript-mode . emmet-mode)
	 (js-mode . emmet-mode)
	 (markdown-mode . emmet-mode)
	 (php . emmet-mode)))

(use-package ace-window
  :bind ("C-x o " . ace-window)
  :config
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 2.0))))))

(use-package projectile
  :config
  (projectile-mode +1)
  (setq projectile-use-git-grep t)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package ivy
  :config (ivy-mode))

(use-package counsel
  :after ivy
  :config (counsel-mode))

(use-package counsel-projectile
  :after (counsel projectile)
  :config (counsel-projectile-mode))

(use-package smex
  :after counsel
  :config (smex-initialize))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
	 ("C-S-s" . swiper-thing-at-point)))

(use-package yasnippet
  :config (yas-global-mode 1))

(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode)
  :config
  (defvar restclient-auth-token nil)
  (defun update-token-restclient-hook ()
    "Update token from a request."
    (save-excursion
      (save-match-data
	;; update regexp to extract required data
	(when (re-search-forward "\"token\":\"\\(.*?\\)\"" nil t)
	  (setq restclient-auth-token (match-string 1))))))
  (add-hook 'restclient-response-received-hook #'update-token-restclient-hook))

(use-package diff-hl
  :config (global-diff-hl-mode))

(use-package hl-todo)

(use-package editorconfig
  :config (editorconfig-mode 1))

(use-package which-key
  :config (which-key-mode))

(use-package json-reformat
  :defer t
  :config (define-key evil-normal-state-map (kbd "C-c j") 'json-reformat-region))

(use-package rainbow-delimiters
  :hook ((js-mode . rainbow-delimiters-mode)
	 (emacs-lisp-mode . rainbow-delimiters-mode)
	 (csharp-mode . rainbow-delimiters-mode)
	 (clojure-mode . rainbow-delimiters-mode)
	 (python-mode . rainbow-delimiters-mode)))

(use-package rainbow-mode
  :hook ((js-mode . rainbow-mode)
	 (clojure-mode . rainbow-mode)
	 (js2-mode . rainbow-mode)
	 (web-mode . rainbow-mode)
	 (css-mode . rainbow-mode)))

(use-package paren
  :ensure nil
  :hook (after-init-hook . show-paren-mode)
  :config
  (custom-set-faces
   '(show-paren-match ((t nil)))
   '(show-paren-match-expression ((t (:background "gray92")))))
  (setq show-paren-style 'expression
	show-paren-when-point-inside-paren nil
	show-paren-when-point-in-periphery t))

(use-package doom-themes
  :config
  ;;(load-theme 'doom-molokai t))
  (load-theme 'doom-one-light t))

(use-package mood-line
  :config (mood-line-mode))

;;; End of packages

;; Set env
(setenv "LANG" "en_US.UTF-8")

;; Make subWordsCountAsEnds
(subword-mode 1)

;; Deleted repeating blank lines
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Remove ugly menu bar
(menu-bar-mode -1)

;; Remove ugly top bar
(tool-bar-mode -1)

;; Remove ugly scroll bar
(scroll-bar-mode -1)

;; Make focus help window on appear
(setq help-window-select t)

;; Remove splash screen
(setq inhibit-splash-screen t)

;; Hide macOS top bar
(setq ns-auto-hide-menu-bar t)

;; Set color of macOS bar
(add-to-list 'default-frame-alist '(ns-appearance . light))

;; Make line numbers 4 digits + space
(setq linum-format "%4d  ")

;; Turn on line numbers
;;(global-linum-mode t)

;; Highlight closing tags (parenthesis, brackets)
(show-paren-mode t)
(setq show-paren-delay 0)

;; Stop process spamming mini buffer
(setq comint-process-echoes t)

;; Set enviroment variables
(setenv "PATH"
	(concat
	 "/usr/local/bin" ";"
	 "/usr/bin" ";"
	 "/bin" ";"
	 "/usr/sbin" ";"
	 "/sbin" ";"
	 (getenv "PATH")))

;; Change font color for keywords
(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode 1))

;; Turn auto reload of buffer (on file change) on
(global-auto-revert-mode t)

;; Enable line wrapping
(global-visual-line-mode)

;; Turn off error sound
(setq ring-bell-function 'ignore)

;; Turn off creation of "~" temp files
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Make shell open in same window
(add-to-list 'display-buffer-alist
	     `(,(regexp-quote "*shell") display-buffer-same-window))

;; Setup indentation
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode t)
(setq tab-width 2)

;; German keyboard binding, by making emasc ignore right-alt key
(setq ns-right-alternate-modifier nil)

;; Bind goto definition
(global-set-key (kbd "C-x j") 'xref-find-definitions)

;; Bind go back (e.g from find definition)
(global-set-key (kbd "C-x p") 'xref-pop-marker-stack)

;; Bind shell
(global-set-key (kbd "C-c b") 'shell)

;; Bind ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Load env related stuff (work vs home)
(let ((path (concat "~/.emacs.d/env/" (or (getenv "emacs-env") "home") ".el")))
  (when (file-exists-p path)
    (load-file path)))

;; Load emacs custom stuff
(load-file "./.emacs.d/custom.el")
