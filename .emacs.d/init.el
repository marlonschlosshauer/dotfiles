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
  :hook  ((typescript-ts-mode . rainbow-delimiters-mode)
          (typescript-mode . rainbow-delimiters-mode)
          (js-ts-mode . rainbow-delimiters-mode)
          (web-mode . rainbow-delimiters-mode)
          (go-ts-mode . rainbow-delimiters-mode)
          (emacs-lisp-mode . rainbow-delimiters-mode)
          (nxml-mode . rainbow-delimiters-mode)))

(use-package rainbow-identifiers
  :hook  ((typescript-ts-mode . rainbow-identifiers-mode)
          (typescript-mode . rainbow-identifiers-mode)
          (js-ts-mode . rainbow-identifiers-mode)
          (web-mode . rainbow-identifiers-mode)
          (go-ts-mode . rainbow-identifiers-mode)
          (emacs-lisp-mode . rainbow-identifiers-mode)
          (nxml-mode . rainbow-identifiers-mode)))

(use-package rainbow-mode
  :hook  ((typescript-ts-mode . rainbow-mode)
          (typescript-mode . rainbow-mode)
          (js-ts-mode . rainbow-mode)
          (web-mode . rainbow-mode)
          (go-ts-mode . rainbow-mode)
          (emacs-lisp-mode . rainbow-mode)
          (nxml-mode . rainbow-mode)))

(use-package paren
  :custom
  (show-paren-style 'expression)
  (show-paren-delay 0)
  (show-paren-when-at-point-inside nil)
  (show-paren-when-at-point-in-periphery t)
  :config
  (show-paren-mode))

(use-package diff-hl
  :init (global-diff-hl-mode))

(use-package mood-line
  :config (mood-line-mode))

(use-package doom-themes
  :custom (custom-safe-themes t)
  :config
  (load-theme 'doom-one-light))

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

;; Hide macOS top bar
(setq ns-auto-hide-menu-bar t)

;; Set color of macOS bar
;;(add-to-list 'default-frame-alist '(ns-appearance . light))

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

;;; Editing
(use-package undo-tree
  :ensure t
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
  :hook ((web-mode . prettier-js-mode)
         (typescript-ts-mode . prettier-js-mode)))

(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
         (typescript-ts-mode . emmet-mode)
         (nxml-mode . emmet-mode))
  :config
  (add-to-list 'emmet-jsx-major-modes 'typescript-ts-mode))

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
  :bind ("TAB" . company-complete)
  :custom
  (company-require-match nil)
  (company-show-numbers t)
  (company-selection-wrap-around t)
  :init
  (global-company-mode))

(use-package lsp-mode
  :commands lsp
  :after (flycheck)
  :hook ((typescript-ts-mode . lsp)
         (js-ts-mode . lsp)
         (web-mode . lsp)
         (go-ts-mode . lsp))
  :bind ("M-?" . lsp-find-references)
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-diagnostics-provider :flycheck)
  (lsp-completion-provider :company)
  (lsp-log-io nil)
  :functions lsp-execute-code-action-by-kind
  :config
  (defun lsp-remove-unused ()
    "Run the remove unused imports code action."
    (interactive)
    (lsp-execute-code-action-by-kind "source.removeUnusedImports.ts")))

(use-package flycheck
  :init (global-flycheck-mode))

;;; Tools
(use-package dired
  :custom
  (dired-listing-switches "-alh"))

(use-package magit
  :bind (("C-c g" . magit))
  :custom
  (magit-display-buffer-function (quote magit-display-buffer-same-window-except-diff-v1)))

(use-package org
  :pin gnu
  :mode (("\\.org\\'" . org-mode))
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
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
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package yasnippet
  :bind (("C-c C-s" . yas-expand))
  :init (yas-global-mode))

(use-package which-key
  :init (which-key-mode))

(use-package gptel
  :bind (("C-c h" . gptel)
         ("C-c c" . gptel-abort)
         ("C-c m" . gptel-menu)
         ("C-c RET" . gpte-send))
  :custom
  (gptel-model "gpt-4o")
  (gptel-api-key (get-authinfo-value "api.openai.com" "apikey")))

(use-package json-reformat
  :defer t
  :bind ("C-c j" . json-reformat-region))

(use-package xml-format
  :defer t)

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
  :init (ivy-mode))

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
         ("C-x C-r" . ivy-resume)
         ("C-S-s" . swiper-thing-at-point)))

(use-package avy
  :bind ("C-SPC" . avy-goto-char))

(use-package ace-window
  :bind ("C-x o " . ace-window)
  :config
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 2.0))))))

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
  (javascript-indent-level 1)
  (js-indent-level 1))

(use-package typescript-mode
  :mode (("\\.ts?\\'" . typescript-ts-mode)
         ("\\.tsx?\\'" . typescript-ts-mode))
  :bind (("C-c r o" . lsp-remove-unused))
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
   (typescript-mode . typescript-ts-mode)))

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

(setenv "PATH"
        (concat
         "/usr/local/bin" ";"
         "/usr/bin" ";"
         "/bin" ";"
         "/usr/sbin" ";"
         "/sbin" ";"
         (getenv "PATH")))

;;; Binds
;; German keyboard binding, by making emasc ignore right-alt key
(setq ns-right-alternate-modifier nil)

;; Bind goto definition
(global-set-key (kbd "C-x j") 'xref-find-definitions)

;; Bind go back (e.g from find definition)
(global-set-key (kbd "C-x p") 'xref-pop-marker-stack)

;; Bind shell
(global-set-key (kbd "C-c s") 'eshell)

;; Replace zap-to-char with zap-up-to-char
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; Bind org meta keys to something more sane for German keyboards
(global-set-key (kbd "M-p") 'org-metaup)
(global-set-key (kbd "M-n") 'org-metadown)

;; Unbind font window
(global-unset-key (kbd "s-t"))

;; Unbind color panel
(global-unset-key (kbd "s-C"))

;; Remove scratch message
(setq initial-scratch-message "\n")

(defun switch-to-scratch-buffer ()
  (interactive)
  (if (get-buffer "*scratch*")
      (switch-to-buffer "*scratch*")
    (progn (create-file-buffer "*scratch*")
           (switch-to-buffer "*scratch*"))))

;; Always have scratch ready
(global-set-key (kbd "C-c v") 'switch-to-scratch-buffer)

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
