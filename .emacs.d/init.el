(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Fix emacs looking for incorrect melpa certifications
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Load packages
(package-initialize)

;; Set default dirs
(setq default-directory "~/")
(setq custom-file-path (concat user-emacs-directory "/custom.el"))
(setq custom-file custom-file-path)
(load "~/.emacs.d/functions.el")

;; Setup use package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)
(setq package-install-upgrade-built-in t)

;;; Graphics
(use-package rainbow-delimiters
  :hook  ((typescript-mode . rainbow-delimiters-mode)
          (js-mode . rainbow-delimiters-mode)
          (web-mode . rainbow-delimiters-mode)
          (go-mode . rainbow-delimiters-mode)
          (emacs-lisp-mode . rainbow-delimiters-mode)
          (nxml-mode . rainbow-delimiters-mode)))

(use-package rainbow-identifiers
  :hook  ((typescript-mode . rainbow-identifiers-mode)
          (js-mode . rainbow-identifiers-mode)
          (web-mode . rainbow-identifiers-mode)
          (go-mode . rainbow-identifiers-mode)
          (emacs-lisp-mode . rainbow-identifiers-mode)
          (nxml-mode . rainbow-identifiers-mode)))

(use-package rainbow-mode
  :hook  ((typescript-mode . rainbow-mode)
          (js-mode . rainbow-mode)
          (web-mode . rainbow-mode)
          (go-mode . rainbow-mode)
          (emacs-lisp-mode . rainbow-mode)
          (nxml-mode . rainbow-mode)))

(use-package paren
  :ensure nil
  :hook (after-init-hook . show-paren-mode)
  :config
  (setq show-paren-style 'expression
        show-paren-when-point-inside-paren nil
        show-paren-when-point-in-periphery t))

(use-package diff-hl
  :config (global-diff-hl-mode))

(use-package hl-todo)

(use-package mood-line
  :config (mood-line-mode))

(use-package doom-themes
  :config
  (setq custom-safe-themes t)
  (load-theme 'doom-one-light))

;; Highlight closing tags (parenthesis, brackets)
(show-paren-mode t)
(setq show-paren-delay 0)

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
  :config (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))
        undo-tree-auto-save-history nil))

(use-package evil
  :after undo-tree
  :defer .1
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :bind (("C-." . comment-or-uncomment-region)
         ("C-," . comment-line)
         ("C-x r" . replace-string))
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
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "jJ" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "Jj" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "JJ" 'evil-normal-state))

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

(use-package prettier-js
  :hook ((web-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)))

(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
         (typescript-mode . emmet-mode)
         (nxml-mode . emmet-mode))
  :config
  (add-to-list 'emmet-jsx-major-modes 'typescript-mode))

(use-package editorconfig
  :config (editorconfig-mode 1))

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
  :config
  (setq company-require-match nil
        company-show-numbers t
        company-selection-wrap-around t)
  (global-company-mode 1))

(use-package lsp-mode
  :commands lsp
  :after (evil flycheck)
  :hook ((typescript-mode . lsp)
         (js-mode . lsp)
         (web-mode . lsp)
         (go-mode . lsp))
  :custom
  (gc-cons-threshold 1073741824/2)
  (read-process-output-max (* 1024 1024))
  :config
  (defun lsp-remove-unused ()
    "Run the remove unused imports code action."
    (interactive)
    (lsp-execute-code-action-by-kind "source.removeUnusedImports.ts"))
  (evil-define-key 'normal lsp-mode-map (kbd "C-l") lsp-command-map)
  (define-key evil-normal-state-map (kbd "g d") 'lsp-goto-implementation)
  (define-key evil-normal-state-map (kbd "g t") 'lsp-goto-type-definition)

  ;; Poor performance with languages that don't provide formatter and have formatter setup (py-yapf)
  ;; (add-hook 'before-save-hook 'lsp-format-buffer)
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-diagnostics-provider :flycheck
        lsp-completion-provider :company
        lsp-log-io nil))

(use-package flycheck
  :init (global-flycheck-mode))

;;; Tools
(use-package dired
  :ensure nil
  :config
  (setq dired-listing-switches "-alh"))

(use-package magit
  :ensure t
  :bind (("C-c g" . magit))
  :config
  (setq magit-display-buffer-function (quote magit-display-buffer-same-window-except-diff-v1)))

(use-package org
  :pin gnu
  :mode (("\\.org\\'" . org-mode))
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :config
  (setq  org-return-follows-link t
        org-src-tab-acts-natively t
        org-src-preserve-indentation nil)

  ;; Make fonts in src buffer look pretty
  (setq org-src-fontify-natively t)

  (setq org-latex-tables-centered nil
        org-tags-column 80)

  ;; Remove line-numbers in agenda
  (add-hook 'org-agenda-mode-hook (lambda() (display-line-numbers-mode -1)))

  ;; Force org to open files in dired, instead of finder
  (add-to-list 'org-file-apps '(directory . emacs)))

(use-package markdown-mode
  :mode (("\\.md?\\'" . markdown-mode)
         ("\\.mdx?\\'" . markdown-mode)))

(use-package projectile
  :config
  (projectile-mode +1)
  (setq projectile-use-git-grep t)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package yasnippet
  :bind (("C-c C-s" . yas-expand))
  :config (yas-global-mode 1))

(use-package which-key
  :config (which-key-mode))

(use-package gptel
  :bind (("C-c h" . gptel)
         ("C-c c" . gptel-abort)
         ("C-c m" . gptel-menu)
         ("C-c RET" . gpte-send))
  :config
  (setq gptel-api-key (get-authinfo-value "api.openai.com" "apikey")))

(use-package json-reformat
  :defer t
  :config (define-key evil-normal-state-map (kbd "C-c j") 'json-reformat-region))

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
  :config (ivy-mode))

(use-package ivy-xref
  :after ivy
  :config
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package counsel
  :after ivy
  :config (counsel-mode))

(use-package counsel-projectile
  :after (counsel projectile)
  :config (counsel-projectile-mode))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-x C-r" . ivy-resume)
         ("C-S-s" . swiper-thing-at-point)))

(use-package ace-window
  :bind ("C-x o " . ace-window)
  :config
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 2.0))))))

;;; Languages
(use-package objc-mode
  :ensure nil
  :mode (("\\.mmm?\\'" . objc-mode)
         ("\\.m?\\'" . objc-mode)))

(use-package yaml-mode)

(use-package groovy-mode
  :mode (("\\.gradle?\\'" . groovy-mode)))

(use-package web-mode
  :mode (("\\.css\\'" . web-mode)
         ("\\.html\\'" . web-mode)
         ("\\.php\\'" . web-mode))
  :config
  (setq web-mode-enable-current-column-highlight t
        web-mode-enable-current-element-highlight t
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package js-mode
  :ensure nil
  :mode (("\\.js?\\'" . js-mode)
         ("\\.cjs?\\'" . js-mode)
         ("\\.jsx?\\'" . js-mode))
  :config
  (setq javascript-indent-level 1)
  (setq js-indent-level 1))

(use-package typescript-mode
  :mode (("\\.ts?\\'" . typescript-mode)
         ("\\.tsx?\\'" . typescript-mode))
  :bind (("C-c r o" . lsp-remove-unused))
  :config
  (setq typescript-indent-level 2)
  (setq typescript-auto-indent-flag t))

(use-package go-mode)

;;; Security
(use-package epa
  :after (exec-path-from-shell)
  :config
  (setq epg-gpg-program "gpg"))

(use-package pinentry
  :after (epa exec-path-from-shell)
  :config
  (pinentry-start))

;; Set env
(setenv "LANG" "en_US.UTF-8")

;; Set enviroment variables
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
(load-file "./.emacs.d/custom.el")
