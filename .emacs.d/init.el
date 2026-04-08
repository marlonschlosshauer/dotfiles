(use-package emacs
	:bind
	(:map
	 global-map
	 (("M-l" . mark-word)
		("M-s" . mark-whole-symbol)
		("s-1" . pair-program-mode)
		("C-M-z" . delete-pair)
		("s-n". end-of-buffer)
		("s-p". beginning-of-buffer)
		("s-," . indent-region)
		("C-c v". scratch-buffer)
		("C-," . comment-line)
		("C-;" . comment-region)
		("C-M-:" . replace-regexp)
		("M-q" . quoted-insert)
		("C-x M-l" . capitalize-dwim)
		("s-t" . nil)
		("s-C" . nil)
		("s-h" . nil)
		("C-h h" . nil)
		("C-x C-z" . nil)
		("C-<end>" . nil)
		("C-<home>". nil)
		("C-<prior>" . nil)
		("C-<next>". nil)))
	:preface
  (defun mark-whole-symbol ()
    "Mark the entire symbol at point."
    (interactive)
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        (goto-char (car bounds))
        (set-mark (cdr bounds)))))
	(define-minor-mode pair-program-mode
		"Make it easier for coworkers to understand what is going on."
		:global t
		(if pair-program-mode
				(progn
					(setq dimmer-fraction 0.4)
					(dimmer-mode 1)
					(global-display-line-numbers-mode 1)
					(global-hl-line-mode 1))
			(dimmer-mode -1)
			(global-display-line-numbers-mode -1)
			(global-hl-line-mode -1)))
	:custom
	(initial-major-mode 'fundamental-mode)
	;; Fix emacs looking for incorrect melpa certifications
	(gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
	;; Turn on upgrades
	(package-install-upgrade-built-in t)
	;; Always install stuff
	(use-package-always-ensure t)
	;; Set custom file
	(custom-file (concat user-emacs-directory "/custom.el"))
	;; Performance
	(gc-cons-threshold (* 100 1024 1024))
	(read-process-output-max (* 4 1024 1024))
	(bidi-display-reordering
	 'left-to-right
	 bidi-paragraph-direction 'left-to-right)
	(bidi-inhibit-bpa t)
	(redisplay-skip-fontification-on-input t)
	;; Set default dirs
	(default-directory "~/")
	;; Make line numbers 4 digits + space
	(linum-format "%4d	")
	;; Make focus help window on appear
	(help-window-select t)
	;; Proportionally resize window
	(window-combination-resize t)
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
	;; Preserve clipboard
	(save-interprogram-paste-before-kill t)
	;; No Duplicates in the Kill Ring
	(kill-do-not-save-duplicates t)
	;; Preserve position on scrolling
	(scroll-preserve-screen-position t)
	;; Repeat pop
	(set-mark-command-repeat-pop t)
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
	;; German keyboard binding, by making emasc ignore right-alt key
	(ns-right-alternate-modifier nil)
	;; Remove scratch message
	(initial-scratch-message "\n")
	;; Remove delete-pair delay
	(delete-pair-blink-delay 0)
	:init
	(add-to-list 'package-archives
							 '("melpa" . "https://melpa.org/packages/") t)
	;; Make macOS bar same color as theme
	(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
	:config
	;; Turn on repeat mode
	(repeat-mode 1)
	;; Capital letters are word seperators
	(global-subword-mode t)
	;; Change font color for keywords
	(global-font-lock-mode t)
	;; Enable line wrapping
	(global-visual-line-mode)
	;; Highlight current line
	;; (global-hl-line-mode)
	;; Turn on line numbers
	;; (global-display-line-numbers-mode)
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

(use-package autorevert
	:custom (global-auto-revert-non-file-buffers t)
	:init (global-auto-revert-mode t))

(use-package savehist
	:init
	(savehist-mode)
	:custom
	(savehist-additional-variables
   '(search-ring regexp-search-ring kill-ring)))

(use-package simpler
	:vc (:url "https://github.com/marlonschlosshauer/simpler.el" :rev :newest)
	:config
	(simpler-mode 1))

(use-package treesit
	:ensure nil
	:custom
	(treesit-language-source-alist
	 '((html "https://github.com/tree-sitter/tree-sitter-html")
		 (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
		 (json "https://github.com/tree-sitter/tree-sitter-json")
		 (markdown "https://github.com/ikatyang/tree-sitter-markdown")
		 (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
		 (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
		 (yaml "https://github.com/ikatyang/tree-sitter-yaml"))))

(use-package expreg
	:bind (("C-=" . expreg-expand)
         ("C--" . expreg-contract)))

(use-package modus-themes
	:config
	(load-theme 'modus-operandi t)
	(custom-set-faces
	 '(line-number ((t (:background unspecified))))
	 '(line-number-current-line ((t (:background unspecified))))
	 '(linum ((t (:background unspecified))))
	 '(fringe ((t (:background unspecified))))
	 '(mode-line ((t (:box nil))))
	 '(mode-line-active ((t (:foreground nil :box nil))))
	 '(mode-line-inactive ((t (:foreground nil :box nil))))))

(use-package rainbow-delimiters
	:hook	((tsx-ts-mode
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
	:hook	((tsx-ts-mode
					 typescript-ts-mode
					 js-ts-mode
					 web-mode
					 scss-mode
					 nxml-mode
					 go-mode
					 emacs-lisp-mode)
					. rainbow-mode))

(use-package diff-hl
	:hook (after-init . global-diff-hl-mode))

(use-package mood-line
	:init (mood-line-mode))

(use-package ace-window
	:bind ("M-o" . ace-window))

(use-package avy
	:bind (("s-." . avy-goto-char)
				 ("s-r" . avy-resume)
				 ("s-t" . avy-goto-char-timer)))

(use-package ivy
	:hook (after-init . ivy-mode)
	:custom
	(ivy-initial-inputs-alist nil)
	(completion-in-region-function #'ivy-completion-in-region)
	(ivy-display-functions-alist
	 '((ivy-completion-in-region . nil)
		 (t . nil)))
	:bind ("C-x C-r" . ivy-resume)
	:config
	(add-to-list 'ivy-sort-functions-alist '(ivy-completion-in-region)))

(use-package swiper
	:after ivy
	:bind (("C-s" . swiper)
				 ("C-M-s" . swiper-all)
				 ("C-S-s" . swiper-thing-at-point)))

(use-package counsel
	:vc (:url "https://github.com/marlonschlosshauer/swiper" :rev :newest)
	:after ivy
	:init (counsel-mode)
	:bind (("s-g" . counsel-global-mark-ring)
				 ("s-G" . counsel-mark-ring)))

(use-package substitute
  :config
	:bind (("C-:" . substitute-target-in-buffer)))

(use-package undo-tree
	:bind (("C-r" . undo-tree-redo)
				 ("C-z" . undo-tree-undo))
	:custom
	(undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
	(undo-tree-auto-save-history nil)
	:hook (after-init . global-undo-tree-mode))

(use-package elec-pair
	:custom
	(electric-pair-pairs
	 '((?\" . ?\")
		 (?\` . ?\`)
		 (?\' . ?\')
		 (?\( . ?\))
		 (?\[ . ?\])
		 (?\< . ?\>)
		 (?\{ . ?\})))
	:hook (after-init . electric-pair-mode))

(use-package prettier-js
	:hook ((tsx-ts-mode
					typescript-ts-mode
					js-ts-mode
					web-mode
					scss-mode
					js-json-mode
					markdown-mode
					jtsx-jsx-mode
					jtsx-tsx-mode)
				 . prettier-js-mode))

(use-package emmet-mode
	:hook ((jtsx-jsx-mode
					jtsx-tsx-mode
					web-mode
					nxml-mode
					markdown-mode)
				 . emmet-mode)
	:init
	(setq emmet-jsx-className-braces? t)
	(setq emmet-jsx-major-modes '(jtsx-jsx-mode jtsx-tsx-mode)))

(use-package editorconfig
	:hook (after-init . editorconfig-mode))

(use-package lsp-mode
	:commands lsp
	:after (flycheck)
	:hook ((typescript-ts-mode . lsp)
				 (tsx-ts-mode . lsp)
				 (js-ts-mode . lsp)
				 (scss-mode . lsp)
				 (web-mode . lsp)
				 (go-mode . lsp))
	:custom
	(lsp-idle-delay 0.1)
	(lsp-headerline-breadcrumb-enable nil)
	(lsp-diagnostics-provider :flycheck)
	(lsp-log-io nil)
	(lsp-eldoc-enable-hover nil)
	(lsp-signature-auto-activate nil)
	(lsp-signature-render-documentation nil)
	(lsp-typescript-suggest-auto-imports t)
	(lsp-typescript-suggestion-actions-enabled t)
	(lsp-typescript-suggest-complete-function-calls t)
	(lsp-clients-typescript-prefer-use-project-ts-server t)
	(lsp-clients-typescript-preferences
	 '((includeCompletionsForImportStatements . t)
		 (includeCompletionsWithSnippetText . t)
		 (includeAutomaticOptionalChainCompletions . t)
		 (importModuleSpecifierPreference . "shortest")
		 (allowRenameOfImportPath . t)))
	:functions
	(lsp-execute-code-action-by-kind
	 lsp-workspaces
	 lsp-make-rename-files-params
	 lsp-request
	 lsp-make-file-rename
	 lsp--path-to-uri
	 lsp--apply-workspace-edit
	 lsp-notify)
	:bind
	(:map
	 lsp-command-map
	 ("g d" . lsp-goto-implementation)
	 ("g t" . lsp-goto-type-definition)
	 ("g r" . lsp-find-references)
	 ("r o" . lsp-remove-unused)
	 ("r a" . lsp-add-missing)
	 ("r f" . lsp-file-rename)
	 ("h h" . lsp-describe-thing-at-point))
	:preface
	(defun lsp-remove-unused ()
		"Run the remove unused imports code action."
		(interactive)
		(lsp-execute-code-action-by-kind "source.removeUnusedImports.ts"))
	(defun lsp-add-missing ()
		"Run the add missing imports code action."
		(interactive)
		(lsp-execute-code-action-by-kind "source.addMissingImports.ts"))
	(defun lsp-file-rename (new-name)
		"Rename the current file and update all LSP references.
Sends workspace/willRenameFiles to get import updates from the
language server, applies them, then renames the file on disk."
		(interactive
		 (let ((old (buffer-file-name)))
			 (unless old (user-error "Buffer is not visiting a file"))
			 (unless (lsp-workspaces) (user-error "No active LSP session"))
			 (list (read-file-name "Rename to: " (file-name-directory old)
														 nil nil (file-name-nondirectory old)))))
		(let* ((old-path (buffer-file-name))
					 (old-dir (file-name-directory old-path))
					 (new-path (expand-file-name new-name))
					 (new-dir (file-name-directory new-path))
					 (params (lsp-make-rename-files-params
										:files (vector (lsp-make-file-rename
																		:oldUri (lsp--path-to-uri old-path)
																		:newUri (lsp--path-to-uri new-path))))))
			(when (string= old-path new-path)
				(user-error "New name is the same as the old name"))
			(make-directory new-dir t)
			(when-let* ((edits (lsp-request "workspace/willRenameFiles" params)))
				(lsp--apply-workspace-edit edits 'rename-file))
			(rename-file old-path new-path t)
			(set-visited-file-name new-path t t)
			(lsp-notify "workspace/didRenameFiles" params)
			(when (and (not (string= old-dir new-dir))
								 (directory-empty-p old-dir))
				(delete-directory old-dir)))))

(use-package flycheck
	:hook (after-init . global-flycheck-mode)
	:custom (flycheck-idle-change-delay 0.1))

(use-package dired
	:ensure nil
	:bind
	(:map
	 dired-mode-map
	 ("." . dired-up-directory))
	:hook
	(dired-mode . dired-hide-details-mode)
	:custom
	(dired-kill-when-opening-new-dired-buffer t)
	(dired-listing-switches "-alh")
	(dired-hide-details-hide-absolute-location t)
	(delete-by-moving-to-trash t))

(use-package diredfl
		:ensure t
		:config
		(diredfl-global-mode 1))

(use-package magit
	:bind (("C-c g" . magit))
	:custom
	(magit-display-buffer-function (quote magit-display-buffer-same-window-except-diff-v1)))

(use-package vdiff
	:config
	(define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map)
	(define-key vdiff-3way-mode-map (kbd "C-c") vdiff-mode-prefix-map))

(use-package vdiff-magit
	:after magit
	:bind (:map magit-mode-map
				("e" . vdiff-magit-dwim)
				("E" . vdiff-magit))
	:config
	(transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
	(transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
	(transient-suffix-put 'magit-dispatch "E" :description "vdiff")
	(transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit))

(use-package org
	:mode (("\\.org\\'" . org-mode))
	:bind
	(("C-c a" . org-agenda)
	 ("C-c c" . org-capture)
	 ("C-c l" . org-store-link)
	 :map
	 org-mode-map
	 ("M-p" . org-metaup)
	 ("M-n" . org-metadown)
	 ("C-c C-s" . org-schedule))
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
	:hook (after-init . projectile-mode)
	:custom
	(projectile-use-git-grep t)
	(projectile-sort-order 'recentf)
	:bind
	(:map
	 projectile-mode-map
	 ("C-c p" . projectile-command-map)))

(use-package counsel-projectile
	:after (counsel projectile)
	:init (counsel-projectile-mode))

(use-package yasnippet
	:hook (after-init . yas-global-mode))

(use-package which-key
	:hook (after-init . which-key-mode))

(use-package multiple-cursors
	:bind (("M-p n" . mc/mark-next-like-this)
				 ("M-p p" . mc/mark-previous-like-this)
				 ("M-p ." . mc/mark-all-like-this)
				 ("M-p d" . mc/mark-all-like-this-dwim)))

(use-package gptel
	:bind (("C-c h" . gptel)
				 :map gptel-mode-map
				 ("C-c RET" . gptel-send)
				 ("C-c r" . gptel-rewrite)
				 ("C-c c" . gptel-abort)
				 ("C-c m" . gptel-menu))
	:config
	(setq
	 gptel-model 'claude-opus-4-6
	 gptel-backend
	 (gptel-make-anthropic
			 "Claude"
		 :stream t
		 :key (let ((match (auth-source-search :host "api.anthropic.com" :user "apikey")))
						(when match
							(let ((secret (plist-get (car match) :secret)))
								(when secret
									(if (functionp secret)
											(funcall secret)
										secret))))))))

(use-package agent-shell
	:demand
	:functions
	(agent-shell--start
	 agent-shell--resolve-preferred-config
	 agent-shell-select-config)
	:preface
	(defun agent-shell-resume ()
		"Start agent-shell and prompt to resume an existing session."
		(interactive)
		(agent-shell--start :config (or (agent-shell--resolve-preferred-config)
																		(agent-shell-select-config :prompt "Resume agent: ")
																		(error "No agent config found"))
												:session-strategy 'prompt))
	:bind (("C-c C-a" . agent-shell)
				 ("C-c C-S-a" . agent-shell-resume)
				 ("C-c C-!" . agent-shell-dwim)
				 :map agent-shell-mode-map
				 ("C-c r" . agent-shell-queue-request))
	:custom
	(agent-shell-preferred-agent-config 'claude-code)
	(agent-shell-header-style nil)
	(agent-shell-show-welcome-message nil)
	(agent-shell-show-busy-indicator nil))

(use-package xml-format
	:defer t)

(use-package eat
	:config
	(eat-eshell-mode))

(use-package eshell
	:functions eshell/clear-scrollback
	:preface
	(defun eshell-clear-scrollback ()
		(interactive)
		(eshell/clear-scrollback))
	:bind (:map eshell-mode-map
							("C-c C-k" . eshell-clear-scrollback))
	:custom
	(eshell-scroll-to-bottom-on-input t)
	(eshell-highlight-prompt t)
	(eshell-cd-on-directory t))

(use-package conch.el
	:vc (:url "https://github.com/marlonschlosshauer/conch.el" :rev :newest)
	:bind ("C-c s" . conch-eshell))

(use-package ivy-xref
	:after ivy
	:custom
	(xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package marginalia
	:custom (marginalia-field-width 240)
	:hook (after-init . marginalia-mode))

(use-package scss-mode
	:vc (:url "https://github.com/marlonschlosshauer/scss-mode"
			 :branch "master"
			 :rev :newest)
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

(use-package jtsx
	:mode (("\\.jsx?\\'" . jtsx-jsx-mode)
				 ("\\.tsx\\'" . jtsx-tsx-mode)
				 ("\\.ts\\'" . jtsx-typescript-mode)
				 ("\\.js\\'" . jtsx-js-mode)
				 ("\\.cjs\\'" . jtsx-js-mode))
	:bind (:map jtsx-tsx-mode-map
							("C-c j r" . jtsx-rename-jsx-element)
							("C-c j w" . jtsx-wrap-in-jsx-element)
							("C-c j o" . jtsx-jump-jsx-opening-tag)
							("C-c j c" . jtsx-jump-jsx-closing-tag)
							("C-c j u" . jtsx-unwrap-jsx)
							("C-c j d n" . jtsx-delete-jsx-node)
							("C-c j d a" . jtsx-delete-jsx-attribute))
	:custom
	(js-indent-level 2)
	(typescript-indent-level 2)
	(typescript-auto-indent-flag t)
	(typescript-ts-mode-indent-offset 2)
	(jtsx-enable-jsx-element-tags-auto-sync t)
	(jtsx-enable-electric-open-newline-between-jsx-element-tags t))

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
	:hook (after-init . pinentry-start))
