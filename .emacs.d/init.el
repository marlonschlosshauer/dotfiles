(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Fix emacs looking for incorrect melpa certifications
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Load packages
(package-initialize)

;; Performance
(setq gc-cons-threshold 100000000)
(setq read-process-output-max 10000000)

;; Set default dirs
(setq default-directory "~/")
(setq custom-file-path (concat user-emacs-directory "/custom.el"))
(setq custom-file custom-file-path)
(let ((path "~/.emacs.d/functions.el"))
  (when (file-exists-p path)
    (load-file path)))

;; Setup use package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq package-install-upgrade-built-in t)

;;; Graphics
(use-package rainbow-delimiters
  :hook  ((tsx-ts-mode . rainbow-delimiters-mode)
          (typescript-ts-mode . rainbow-delimiters-mode)
          (js-ts-mode . rainbow-delimiters-mode)
          (web-mode . rainbow-delimiters-mode)
          (scss-mode . rainbow-delimiters-mode)
          (nxml-mode . rainbow-delimiters-mode)
          (go-mode . rainbow-delimiters-mode)
          (emacs-lisp-mode . rainbow-delimiters-mode)))

(use-package rainbow-identifiers
  :hook  ((tsx-ts-mode . rainbow-identifiers-mode)
          (typescript-ts-mode . rainbow-identifiers-mode)
          (js-ts-mode . rainbow-identifiers-mode)
          (web-mode . rainbow-identifiers-mode)
          (scss-mode . rainbow-identifiers-mode)
          (nxml-mode . rainbow-identifiers-mode)
          (go-mode . rainbow-identifiers-mode)
          (emacs-lisp-mode . rainbow-identifiers-mode)))

(use-package rainbow-mode
  :hook  ((tsx-ts-mode . rainbow-mode)
          (typescript-ts-mode . rainbow-mode)
          (js-ts-mode . rainbow-mode)
          (web-mode . rainbow-mode)
          (scss-mode . rainbow-mode)
          (nxml-mode . rainbow-mode)
          (go-mode . rainbow-mode)
          (emacs-lisp-mode . rainbow-mode)))

(use-package diff-hl
  :init (global-diff-hl-mode))

(use-package mood-line
  :init (mood-line-mode))

(use-package ef-themes
  :demand t)

(use-package theme-buffet
  :after (ef-themes)
  :custom
  (theme-buffet-end-user
   '(:night
     (ef-autumn
      ef-duo-dark
      ef-night
      ef-tritanopia-dark
      ef-winter
      ef-dark)
     :twilight
     (ef-bio
      ef-cherie)
     :morning
     (ef-elea-light
      ef-maris-light
      ef-spring
      ef-tritanopia-light)
     :day
     (ef-deuteranopia-light
      ef-frost
      ef-light
      ef-trio-light)
     :afternoon
     (ef-cyprus
      ef-arbutus
      ef-day
      ef-duo-light
      ef-kassio
      ef-melissa-light
      ef-summer)
     :evening
     (ef-deuteranopia-dark
      ef-elea-dark
      ef-maris-dark
      ef-melissa-dark
      ef-symbiosis
      ef-trio-dark)))
  :config
  (theme-buffet-end-user)
  (theme-buffet-a-la-carte))

;; Change font color for keywords
(global-font-lock-mode t)

;; Enable line wrapping
(global-visual-line-mode)

;; Highlight current line
(global-hl-line-mode)

;; Make line numbers 4 digits + space
(setq linum-format "%4d  ")

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

;; Make focus help window on appear
(setq help-window-select t)

;; Remove splash screen
(setq inhibit-splash-screen t)

;; Make macOS bar same color as theme
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;; Turn off title bar icon
(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)

;; Stop frames from opening in other windows!
(setq display-buffer-base-action
      '((display-buffer-reuse-window
         display-buffer-reuse-mode-window
         display-buffer-same-window
         display-buffer-in-previous-window)))

;; Make confirmations less annoying
(setq use-dialog-box nil)

;;; Movement
(global-set-key (kbd "M-SPC") 'pop-global-mark)
(global-set-key (kbd "M-l") 'mark-word)
(global-set-key (kbd "C-M-l") 'set-mark-command)
(global-set-key (kbd "M-n") 'scroll-up-command)
(global-set-key (kbd "M-p") 'scroll-down-command)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "s-n") 'end-of-buffer)
(global-set-key (kbd "s-p") 'beginning-of-buffer)

;; Preserve position on scrolling
(setq scroll-preserve-screen-position t)

;; Repeat pop
(setq-default set-mark-command-repeat-pop t)

(use-package avy
  :bind (("s-." . avy-goto-char)
         ("s-r" . avy-resume)
         ("s-g" . avy-goto-line)))

;;; Editing
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
  :hook ((tsx-ts-mode . prettier-js-mode)
         (typescript-ts-mode . prettier-js-mode)
         (js-ts-mode . prettier-js-mode)
         (web-mode . prettier-js-mode)
         (scss-mode . prettier-js-mode)))

(use-package emmet-mode
  :hook ((tsx-ts-mode . emmet-mode)
         (web-mode . emmet-mode)
         (scss-mode . emmet-mode)
         (nxml-mode . emmet-mode)))

(use-package editorconfig
  :init (editorconfig-mode))

;; Make highlighted text be replaced if something is typed
(delete-selection-mode 1)

;; Enable x-case-region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; Deleted repeating blank lines
;;(add-hook 'before-save-hook 'whitespace-cleanup)

;; Make backspace erase tab
(setq backward-delete-char-untabify-method 'hungry)

;; Setup indentation
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode t)
(setq tab-width 2)

;;; Projects
(use-package company
  :bind (:map company-mode-map
              ("TAB" . company-complete))
  :custom
  (company-require-match nil)
  (company-show-numbers t)
  (company-selection-wrap-around t))

(use-package lsp-mode
  :commands lsp
  :after (flycheck)
  :hook ((tsx-ts-mode . lsp)
         (typescript-ts-mode . lsp)
         (js-ts-mode . lsp)
         (web-mode . lsp)
         (go-mode . lsp))
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-diagnostics-provider :flycheck)
  (lsp-completion-provider :company)
  (lsp-log-io nil)
  :functions lsp-execute-code-action-by-kind
  :bind (:map lsp-command-map
              ("g d" . lsp-goto-implementation)
              ("g t" . lsp-goto-type-definition)
              ("g r" . lsp-find-references)
              ("r o" . lsp-remove-unused))
  :config
  (company-mode 1)
  (defun lsp-remove-unused ()
    "Run the remove unused imports code action."
    (interactive)
    (lsp-execute-code-action-by-kind "source.removeUnusedImports.ts")))

(use-package flycheck
  :init (global-flycheck-mode))

;;; Tools
(use-package dired
  :bind (:map dired-mode-map
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
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         :map org-mode-map
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
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(use-package yasnippet
  :bind (:map yas-minor-mode-map
              ("C-c C-s" . yas-expand))
  :init (yas-global-mode))

(use-package which-key
  :init (which-key-mode))

(use-package gptel
  :bind (("C-c h" . gptel)
         ("C-c RET" . gptel-send)
         ("C-c r" . gptel-rewrite)
         ("C-c c" . gpte-abort)
         ("C-c m" . gptel-menu))
  :custom
  (gptel-api-key (get-authinfo-value "api.openai.com" "apikey")))

(use-package xml-format
  :defer t)

(use-package eat
  :config
  (eat-eshell-mode))

;; Stop process spamming mini buffer
(setq comint-process-echoes t)

;; Turn auto reload of buffer (on file change) on
(global-auto-revert-mode t)

;; Turn off error sound
(setq ring-bell-function 'ignore)

;; Turn off creation of "~" temp files
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;;; Navigation
(use-package ivy
  :init (ivy-mode)
  :custom (ivy-initial-inputs-alist nil)
  :bind ("C-x C-r" . ivy-resume))

(use-package ivy-xref
  :after ivy
  :custom
  (xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package counsel
  :after ivy
  :init (counsel-mode))

(use-package counsel-projectile
  :after (counsel projectile)
  :init (counsel-projectile-mode))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-S-s" . swiper-thing-at-point)))

(use-package marginalia
  :init (marginalia-mode))

(use-package savehist
  :init (savehist-mode))

(use-package smartparens
  :hook ((tsx-ts-mode . smartparens-mode)
         (typescript-ts-mode . smartparens-mode)
         (js-ts-mode . smartparens-mode)
         (go-ts-mode . smartparens-mode))
  :bind (("C-M-f" . sp-forward-sexp)
         ("C-M-b" . sp-backward-sexp)))

;;; Languages
(use-package objc-mode
  :mode (("\\.mmm?\\'" . objc-mode)
         ("\\.m?\\'" . objc-mode)))

(use-package yaml-mode
  :mode (("\\.yml?\\'" . yaml-mode)))

(use-package groovy-mode
  :mode (("\\.gradle?\\'" . groovy-mode)))

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

(use-package go-mode)

;;; Security
(use-package epa
  :custom
  (epg-gpg-program "gpg"))

(use-package pinentry
  :after epa
  :init
  (pinentry-start))

;; Grammar
(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (js-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (tsx-mode . tsx-ts-mode)))

(setq treesit-language-source-alist
      '((html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;;; Env
(setenv "LANG" "en_US.UTF-8")

;;; Binds
;; German keyboard binding, by making emasc ignore right-alt key
(setq ns-right-alternate-modifier nil)

(global-set-key (kbd "C-c v") 'scratch-buffer)

(global-unset-key (kbd "s-t"))

(global-unset-key (kbd "s-C"))

;; Remove scratch message
(setq initial-scratch-message "\n")

;;; End of packages
;; Load env related stuff (work vs home)
(let ((path (concat "~/.emacs.d/env/" (or (getenv "emacs-env") "home") ".el")))
  (when (file-exists-p path)
    (load-file path)))

;; Create custom file if not there
(if (not (file-exists-p custom-file-path))
    (write-region "" nil custom-file-path))

;; Load emacs custom stuff
(when (file-exists-p custom-file-path)
  (load-file custom-file-path))
