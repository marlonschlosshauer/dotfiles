(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(package-initialize)

(setq default-directory "~/")
(setq custom-file (concat user-emacs-directory "/custom.el"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package exec-path-from-shell
  :ensure t
  :init
  (setenv "SHELL" "/usr/local/bin/zsh")
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package dired
  :config
  (setq dired-listing-switches "-alh"))

(use-package magit
  :ensure t
  :bind (("C-c g" . magit))
  :config
  (use-package forge
    :ensure t))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

(use-package evil
  :ensure t
  :after undo-tree
  :defer .1
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :bind (("C-l" . nil) ; Unset non-prefix
	 ("C-l k" . comment-or-uncomment-region))
  :config
  (define-key evil-normal-state-map (kbd "B") 'evil-beginning-of-visual-line)
  (define-key evil-normal-state-map (kbd "E") 'evil-end-of-visual-line)

  (evil-mode 1)
  (subword-mode 1)

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

  (use-package evil-surround
    :ensure t
    :after evil
    :config
    (global-evil-surround-mode 1))

  (use-package evil-matchit
    :ensure t
    :hook ((js-mode . evil-matchit-mode)
	   (typescript-mode . evil-matchit-mode)
	   (web-mode . evil-matchit-mode)))

  (use-package key-chord
    :ensure t
    :after evil
    :config
    (key-chord-mode 1)
    (key-chord-define evil-insert-state-map "jj" 'evil-normal-state))

  (use-package elec-pair
    :ensure t
    :config
    (setq electric-pair-pairs
	  '(
	    (?\" . ?\")
	    (?\' . ?\')
	    (?\( . ?\))
	    (?\[ . ?\])
	    (?\{ . ?\})))
    (electric-pair-mode 1))

  (use-package ace-jump-mode
    :config
    (define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)))

(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode)
  (use-package tree-sitter-langs
    :ensure t
    :after tree-sitter))

(use-package org
  :pin gnu
  :mode (("\\.org\\'" . org-mode))
  :defer t
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

  (setq org-latex-listings 'minted)
  (setq org-latex-minted-options
	'(("breaklines=true")))

  ;; TODO: find out how to change from bibtex to biber (cuz bibtex does not support biblatex anymore)
  (setq bibtex-dialect 'biblatex)

  ;; (setq org-latex-pdf-process
  ;;	'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;;	  "bibtex %f"
  ;;	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;; this works for with org-ref for some reason
  (setq org-latex-pdf-process '("latexmk -pdflatex='%latex -shell-escape -interaction nonstopmode' -pdf -output-directory=%o -f %f"))

  (setq org-src-fontify-natively t)

  (add-hook 'org-agenda-mode-hook (lambda() (linum-mode -1)))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (shell . t)
     (python . t)))

  ;; Force org to open files in dired, instead of finder
  (add-to-list 'org-file-apps '(directory . emacs))

  (setq org-agenda-files '("~/Library/CloudStorage/Dropbox/org/todo.org"
			   "~/Library/CloudStorage/Dropbox/gedankenessen/todo.org"
			   "~/Library/CloudStorage/Dropbox/neo/todo.org"
			   "~/Library/CloudStorage/Dropbox/TP/todo.org"))

  (setq org-default-notes-file "~/Library/CloudStorage/Dropbox/org/todo.org")
  (setq org-capture-templates
	'(("t" "TODO" entry (file+headline "~/Library/CloudStorage/Dropbox/org/todo.org" "Todo")
	   "* TODO %? %i\n  %a")
	  ("p" "TP" entry (file+headline "~/Library/CloudStorage/Dropbox/TP/todo.org" "Todo")
	   "* TODO %? \n %U")
	  ("g" "gde" entry (file+headline "~/Library/CloudStorage/Dropbox/gedankenessen/todo.org" "Todo")
	   "* TODO %? \n %U")
	  ("n" "neo" entry (file+headline "~/Library/CloudStorage/Dropbox/neo/todo.org" "Todo")
	   "* TODO %? %i\n  %a")))

  (use-package org-pdfview
    :disabled
    :ensure t
    :after pdf-tools)

  (use-package org-ref
    :ensure t))

(use-package pdf-tools
  :ensure t
  :config
  (add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1))))

(use-package elfeed
  :defer t
  :commands elfeed
  :after (elfeed-org, evil-collection)
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
    (setq rmh-elfeed-org-files (list "~/Dropbox/org/rss.org"))
    (elfeed-org)))

(use-package company
  :defer t
  :bind ("TAB" . company-complete)
  :config
  (setq company-require-match nil)
  (setq company-show-numbers t)
  (setq company-selection-wrap-around t)
  (global-company-mode 1)

  (use-package company-restclient
    :ensure t)
  (use-package company-web
    :ensure t))

(use-package lsp-mode
  :defer t
  :commands lsp
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
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-log-io nil)

  (use-package lsp-ui
    :ensure t
    :after lsp-mode
    :config (setq lsp-ui-doc-enable t
		  lsp-ui-peek-enable t
		  lsp-ui-sideline-enable nil
		  lsp-ui-doc-position (quote at-point))))

(use-package lsp-java
  :ensure t
  :after (java-mode, lsp))

(use-package flycheck
  :ensure t
  :bind ("C-c n" . flycheck-next-error)
  :init (global-flycheck-mode))

(use-package clojurescript-mode
  :defer t
  :mode (("\\.cljs?\\'" . clojurescript-mode)))

(use-package clojure-mode
  :defer t
  :mode (("\\.clj?\\'" . clojure-mode)))

(use-package evil-cleverparens
  :defer t
  :hook ((clojure-mode . evil-cleverparens-mode)
	 (clojurescript-mode . evil-cleverparens-mode)))

(use-package cider
  :bind ("C-l" . cider-repl-clear-buffer)
  :config
  (setq cider-show-error-buffer 'only-in-repl))

(use-package objc-mode
  :mode (("\\.mmm?\\'" . objc-mode)
	 ("\\.m?\\'" . objc-mode)))

(use-package groovy-mode
  :mode (("\\.gradle?\\'" . groovy-mode)))

(use-package css-mode
  :defer t
  :mode (("\\.css\\'" . css-mode)))

(use-package web-mode
  :defer t
  :mode (("\\.html\\'" . web-mode)
	 ("\\.php\\'" . web-mode))

  :config
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t)

  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package js-mode
  :defer t
  :mode (("\\.js?\\'" . js-mode)
	 ("\\.jsx?\\'" . js-mode))
  :config
  (setq javascript-indent-level 2)
  (setq js-indent-level 2))

(use-package typescript-mode
  :defer t
  :mode (("\\.ts?\\'" . typescript-mode)
	 ("\\.tsx?\\'" . typescript-mode))
  :config
  (setq typescript-indent-level 2)
  (setq typescript-auto-indent-flag t))

(use-package emmet-mode
  :defer t
  :config
  :hook ((web-mode . emmet-mode)
	 (typescript-mode . emmet-mode)
	 (js-mode . emmet-mode)
	 (markdown-mode . emmet-mode)
	 (php . emmet-mode)))

(use-package ace-window
  :ensure t
  :bind ("C-x o " . ace-window)
  :config
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 2.0))))))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (setq projectile-use-git-grep t)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package ivy
  :ensure t
  :config
  (ivy-mode)

  (use-package counsel
    :ensure t
    :config
    (counsel-mode)

    (use-package counsel-projectile
      :ensure t
      :after projectile
      :config
      (counsel-projectile-mode))

    (use-package smex
      :config
      (smex-initialize)))

  (use-package swiper
    :ensure t
    :bind (("C-s" . swiper)("C-S-s" . swiper-thing-at-point))))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package restclient
  :defer t
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
  :ensure t
  :config
  (global-diff-hl-mode))

(use-package hl-todo
  :ensure t)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package json-reformat
  :ensure t
  :after evil
  :config
  (define-key evil-normal-state-map (kbd "C-c j") 'json-reformat-region))

(use-package rainbow-delimiters
  :ensure
  :hook ((js-mode . rainbow-delimiters-mode)
	 (emacs-lisp-mode . rainbow-delimiters-mode)
	 (csharp-mode . rainbow-delimiters-mode)
	 (clojure-mode . rainbow-delimiters-mode)
	 (python-mode . rainbow-delimiters-mode)))

(use-package rainbow-mode
  :ensure
  :hook ((js-mode . rainbow-mode)
	 (clojure-mode . rainbow-mode)
	 (js2-mode . rainbow-mode)
	 (web-mode . rainbow-mode)
	 (css-mode . rainbow-mode)))

(use-package paren
  :config
  (setq show-paren-style 'expression)
  (setq show-paren-when-point-in-periphery t)
  (setq show-paren-when-point-inside-paren nil)
  :hook (after-init-hook . show-paren-mode))

(use-package doom-themes
  :ensure t
  :config
  ;;(load-theme 'doom-molokai t))
  (load-theme 'doom-one-light t))

(use-package mood-line
  :ensure t
  :config
  (mood-line-mode))

;;; End of packages

;;; Misc
(setenv "LANG" "en_US.UTF-8")

;; Deleted repeating blank lines
(add-hook 'before-save-hook 'whitespace-cleanup)
;;; End of Misc

;;; GUI
;; Removing GUI items
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;; End of GUI

;; Make focus help window on appear
(setq help-window-select t)

;; Remove splash screen
(setq inhibit-splash-screen t)

;; Hide macOS top bar
(setq ns-auto-hide-menu-bar t)

;; Set color of macOS bar
(add-to-list 'default-frame-alist '(ns-appearance . light))

;; line numbers
;;(setq linum-format "%4d  ")
;;(global-linum-mode t)

;; Highlight closing tags (parenthesis, brackets)
(show-paren-mode t)
(setq show-paren-delay 0)

(setq comint-process-echoes t)

;;; Built-in Emacs
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

(add-to-list 'display-buffer-alist
	     `(,(regexp-quote "*shell") display-buffer-same-window))

;; Setup indentation
(setq indent-tabs-mode t)
(setq tab-width 2)

;;; Binds
;; German keyboard binding, by making emasc ignore right-alt key
(setq ns-right-alternate-modifier nil)

(global-set-key (kbd "C-x j") 'xref-find-definitions)
(global-set-key (kbd "C-x p") 'xref-pop-marker-stack)
(global-set-key (kbd "C-c b") 'shell)
(global-set-key (kbd "C-x C-b") 'ibuffer)
;;; End of Binds

(load-file "./.emacs.d/custom.el")
