(use-package emacs
  :bind
  (:map
   global-map
   (("M-SPC" . pop-global-mark)
    ("M-l" . mark-word)
    ("C-M-l".  set-mark-command)
    ("M-n". scroll-up-command)
    ("M-p". scroll-down-command)
    ("M-z". zap-up-to-char)
    ("M-g". goto-line)
    ("s-n". end-of-buffer)
    ("s-p". beginning-of-buffer)
		("s-," . indent-region)
    ("C-c v". scratch-buffer)
		("C-:" . replace-regexp)
		("C-," . comment-line)
		("C-;" . comment-region)
		("C-z". undo)
		("C-r" . undo-redo)
    ("s-t" . nil)
    ("s-C" . nil)
		("s-h" . nil)
    ("C-<end>" . nil)
    ("C-<home>". nil)
    ("C-<prior>" . nil)
    ("C-<next>". nil)))
  :custom
	(initial-major-mode 'org-mode)
  ;; Fix emacs looking for incorrect melpa certifications
  (gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
  ;; Turn on upgrades
  (package-install-upgrade-built-in t)
	;; Always install stuff
	(use-package-always-ensure t)
	;; Set custom file
	(custom-file (concat user-emacs-directory "/custom.el"))
  ;; Performance
  (gc-cons-threshold 100000000)
  (read-process-output-max 10000000)
  ;; Set default dirs
  (default-directory "~/")
  ;; Make line numbers 4 digits + space
  (linum-format "%4d  ")
  ;; Make focus help window on appear
  (help-window-select t)
  ;; Remove splash screen
  (inhibit-splash-screen t)
  ;; Turn off title bar icon
  (ns-use-proxy-icon nil)
  ;; Remove title
  ( frame-title-format nil)
  ;; Stop frames from opening in other windows!
  (display-buffer-base-action
   '((display-buffer-reuse-window
      display-buffer-reuse-mode-window
      display-buffer-same-window
      display-buffer-in-previous-window)))
  ;; Make confirmations less annoying
  (use-dialog-box nil)
  ;; Preserve position on scrolling
  (scroll-preserve-screen-position t)
  ;; Repeat pop
  ( set-mark-command-repeat-pop t)
  ;; Make backspace erase tab
  (backward-delete-char-untabify-method 'hungry)
  ;; Setup indentation
  (indent-tabs-mode t)
  (tab-width 2)
  ;; Stop process spamming mini buffer
  (comint-process-echoes t)
  ;; Turn off error sound
  (ring-bell-function 'ignore)
  ;; Turn off creation of "~" temp files
  (make-backup-files nil)
  (auto-save-default nil)
  (create-lockfiles nil)
	;; Save mini-buffer history
	(savehist-mode)
  ;; Grammar
  (major-mode-remap-alist
   '((yaml-mode . yaml-ts-mode)
     (js-mode . js-ts-mode)
     (typescript-mode . typescript-ts-mode)
     (tsx-mode . tsx-ts-mode)))
  ;; Set grammar sources
  (treesit-language-source-alist
   '((html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  ;; German keyboard binding, by making emasc ignore right-alt key
  (ns-right-alternate-modifier nil)
  ;; Remove scratch message
  (initial-scratch-message "\n")
  :init
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)
  ;; Make macOS bar same color as theme
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  :config
	;; Capital letters are word seperators
	(global-subword-mode t)
  ;; Change font color for keywords
  (global-font-lock-mode t)
  ;; Enable line wrapping
  (global-visual-line-mode)
  ;; Highlight current line
  (global-hl-line-mode)
  ;; Turn on line numbers
  (global-display-line-numbers-mode)
  ;; Set font size 18pt
  (set-face-attribute 'default nil :height 180)
  ;; Remove ugly menu bar
  (menu-bar-mode -1)
  ;; Remove ugly top bar
  (tool-bar-mode -1)
  ;; Remove ugly scroll bar
  (scroll-bar-mode -1)
  ;; Turn auto reload of buffer (on file change) on
  (global-auto-revert-mode t)
  ;; Make highlighted text be replaced if something is typed
  (delete-selection-mode 1)
  (setenv "LANG" "en_US.UTF-8")
  ;; Enable functions
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
	;; Create custom file if not there
	(if (not (file-exists-p custom-file))
			(write-region "" nil custom-file))
	;; Load emacs custom stuff
	(when (file-exists-p custom-file)
		(load-file custom-file))
  ;; Load env related stuff (work vs home)
  (let ((path (concat user-emacs-directory "/env/" (or (getenv "emacs-env") "home") ".el")))
    (when (file-exists-p path)
      (load-file path))))

(use-package rainbow-delimiters
  :hook  ((tsx-ts-mode
           typescript-ts-mode
           js-ts-mode
           web-mode
					 scss-mode
           nxml-mode
           go-mode
           emacs-lisp-mode)
					. rainbow-delimiters-mode))

(use-package rainbow-identifiers
  :hook ((tsx-ts-mode
          typescript-ts-mode
          js-ts-mode
          web-mode
					scss-mode
          nxml-mode
          go-mode
          emacs-lisp-mode)
         . rainbow-identifiers-mode))

(use-package rainbow-mode
  :hook  ((tsx-ts-mode
           typescript-ts-mode
           js-ts-mode
           web-mode
					 scss-mode
           nxml-mode
           go-mode
           emacs-lisp-mode)
					. rainbow-mode))

(use-package diff-hl
  :init (global-diff-hl-mode))

(use-package mood-line
  :init (mood-line-mode))

(use-package ef-themes
  :demand t)

(use-package theme-buffet
  :after (ef-themes)
  :config
  (theme-buffet-modus-ef)
	(theme-buffet-a-la-carte))

(use-package avy
  :bind (("s-." . avy-goto-char)
         ("s-r" . avy-resume)
         ("s-g" . avy-goto-line)))

(use-package ivy
  :init (ivy-mode)
  :custom (ivy-initial-inputs-alist nil)
  :bind ("C-x C-r" . ivy-resume))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-S-s" . swiper-thing-at-point)))

(use-package counsel
  :after ivy
  :init (counsel-mode))

(use-package back-button
	:bind	(("s-f" . back-button-local-forward)
				 ("s-b" . back-button-local-backward)
				 ("s-F" . back-button-global-forward)
				 ("s-B" . back-button-global-backward)))

(use-package undo-tree
  :custom
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (undo-tree-auto-save-history nil)
  :init (global-undo-tree-mode))

(use-package elec-pair
  :custom
  (electric-pair-pairs
   '((?\" . ?\")
     (?\' . ?\')
     (?\( . ?\))
     (?\[ . ?\])
     (?\< . ?\>)
     (?\{ . ?\})))
  :init (electric-pair-mode))

(use-package prettier-js
  :hook ((tsx-ts-mode
          typescript-ts-mode
          js-ts-mode
          web-mode
					scss-mode
					js-json-mode)
         . prettier-js-mode))

(use-package emmet-mode
  :hook ((tsx-ts-mode
          web-mode
          nxml-mode)
         . emmet-mode))

(use-package editorconfig
  :init (editorconfig-mode))

(use-package lsp-mode
  :commands lsp
  :after (flycheck)
  :hook ((tsx-ts-mode . lsp)
         (typescript-ts-mode . lsp)
         (js-ts-mode . lsp)
				 (scss-mode . lsp)
         (web-mode . lsp)
         (go-mode . lsp))
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-diagnostics-provider :flycheck)
  (lsp-log-io nil)
	(lsp-clients-typescript-prefer-use-project-ts-server t)
  :functions lsp-execute-code-action-by-kind
  :bind
	(:map
	 lsp-command-map
   ("g d" . lsp-goto-implementation)
   ("g t" . lsp-goto-type-definition)
   ("g r" . lsp-find-references)
   ("r o" . lsp-remove-unused))
  :config
  (defun lsp-remove-unused ()
    "Run the remove unused imports code action."
    (interactive)
    (lsp-execute-code-action-by-kind "source.removeUnusedImports.ts")))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package dired
  :bind
	(:map
	 dired-mode-map
   ("." . dired-up-directory))
  :custom
  (dired-listing-switches "-alh"))

(use-package eshell
  :bind ("C-c s" . eshell))

(use-package magit
  :bind (("C-c g" . magit))
  :custom
  (magit-display-buffer-function (quote magit-display-buffer-same-window-except-diff-v1)))

(use-package org
  :mode (("\\.org\\'" . org-mode))
  :bind
	(("C-c a" . org-agenda)
	 ("C-c c" . org-capture)
	 ("C-c l" . org-store-link)
	 :map
	 org-mode-map
	 ("M-p" . org-metaup)
	 ("M-n" . org-metadown))
  :custom
  (org-return-follows-link t)
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation nil)
  (org-src-fontify-natively t)
  :config
  ;; Remove line-numbers in agenda
  (add-hook 'org-agenda-mode-hook (lambda() (display-line-numbers-mode -1)))
  ;; Force org to open files in dired, instead of finder
  (add-to-list 'org-file-apps '(directory . emacs)))

(use-package markdown-mode
  :mode (("\\.md?\\'" . markdown-mode)
         ("\\.mdx?\\'" . markdown-mode)))

(use-package projectile
  :init (projectile-mode)
  :custom (projectile-use-git-grep t)
  :bind
	(:map
	 projectile-mode-map
   ("C-c p" . projectile-command-map)))

(use-package counsel-projectile
  :after (counsel projectile)
  :init (counsel-projectile-mode))

(use-package yasnippet
  :bind
	(:map
	 yas-minor-mode-map
   ("C-c C-s" . yas-expand))
  :init (yas-global-mode))

(use-package which-key
  :init (which-key-mode))

(use-package gptel
  :bind (("C-c h" . gptel)
         ("C-c RET" . gptel-send)
         ("C-c r" . gptel-rewrite)
         ("C-c c" . gptel-abort)
         ("C-c m" . gptel-menu))
	:custom
	(gptel-model 'o3-mini)
	(gptel-api-key
   (let ((match (auth-source-search :host "api.openai.com" :user "apikey")))
     (when match
       (let ((secret (plist-get (car match) :secret)))
         (when secret
           (if (functionp secret)
               (funcall secret)
             secret)))))))

(use-package xml-format
  :defer t)

(use-package eat
  :config
  (eat-eshell-mode))

(use-package ivy-xref
  :after ivy
  :custom
  (xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package marginalia
  :init (marginalia-mode))

(use-package scss-mode
	:mode (("\\.scss\\'" . scss-mode)))

(use-package web-mode
  :mode (("\\.css\\'" . web-mode)
         ("\\.html\\'" . web-mode)
         ("\\.php\\'" . web-mode))
  :custom
  (web-mode-enable-current-column-highlight t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

(use-package js-mode
  :mode (("\\.js?\\'" . js-mode)
         ("\\.cjs?\\'" . js-mode)
         ("\\.jsx?\\'" . js-mode))
  :custom
  (js-indent-level 2))

(use-package typescript-mode
  :mode (("\\.ts?\\'" . typescript-ts-mode)
         ("\\.tsx?\\'" . tsx-ts-mode))
  :custom
  (typescript-indent-level 2)
  (typescript-auto-indent-flag t))

(use-package clojure-ts-mode
	:custom
	(clojure-ts-comment-macro-font-lock-body t)
	(clojure-ts-toplevel-inside-comment-form t))

(use-package cider
	:custom
	(cider-show-error-buffer 'only-in-repl)
	:bind
	(:map
	 cider-mode-map
	 ("C-l" . cider-repl-clear-buffer)))

(use-package epa
  :custom
  (epg-gpg-program "gpg"))

(use-package pinentry
  :after epa
  :init
  (pinentry-start))
