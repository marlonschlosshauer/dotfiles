(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(package-initialize)

(setq default-directory "~/")

					;(unless (package-installed-p 'use-package)
					;  (package-refresh-contents)
					;  (package-install 'use-package))

(use-package exec-path-from-shell
  :ensure t
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

  (use-package key-chord
    :ensure t
    :after evil
    :config
    (key-chord-mode 1)
    (key-chord-define evil-insert-state-map "jj" 'evil-normal-state))

  (use-package expand-region
    :config
    (define-key evil-normal-state-map (kbd "s-l") 'er/expand-region))

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

(use-package json-reformat
  :ensure t
  :after evil
  :config
  (define-key evil-normal-state-map (kbd "C-c j") 'json-reformat-region))

(use-package org
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

  (setq org-latex-pdf-process
	'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "bibtex %f"
	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  (setq org-src-fontify-natively t)

  (add-hook 'org-agenda-mode-hook (lambda() (linum-mode -1)))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (shell . t)
     (python . t)))

  ;; Force org to open files in dired, instead of finder
  (add-to-list 'org-file-apps '(directory . emacs))
  (add-to-list 'org-src-lang-modes (cons "jsx" 'rjsx))

  (setq org-agenda-files '("~/Dropbox/org/todo.org" "~/Projects/p2/todo.org" "~/Dropbox/Study" "~/Dropbox/Study/Labs/TH/todo.org" "~/Projects/gde/todo.org"))
  (setq org-default-notes-file "~/Dropbox/org/todo.org")
  (setq org-agenda-custom-commands
	'(("c" "Simple agenda view"
	   ((agenda "")
	    (alltodo "")))))

  (setq org-capture-templates
	'(("t" "TODO" entry (file+headline "~/Dropbox/org/todo.org" "Todo")
	   "* TODO %? %i\n  %a")
	  ("h" "TH" entry (file+headline "~/Dropbox/Study/Labs/TH/todo.org" "Todo")
	   "* TODO %? \n %U")
	  ("g" "gedankenessen" entry (file+headline "~/Projects/gde/todo.org" "Todo")
	   "* TODO %? \n %U")
	  ("s" "Studium" entry (file+headline "~/Dropbox/Study/todo.org" "Todo")
	   "* TODO %? %i\n  %a")))

  (use-package org-pdfview
    :disabled
    :ensure t
    :after pdf-tools)

  (use-package org-ref
    :disabled
    :ensure nil))

(use-package pdf-tools
  :ensure t
  :config
  (add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1))))


(use-package elfeed
  :ensure t
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

(use-package pocket-reader
  :ensure t
  :config
  (define-key pocket-reader-mode-map (kbd "RET") 'pocket-reader-open-in-external-browser))

(use-package company
  :ensure t
  :bind ("C-SPC" . company-complete)
  :config
  (global-company-mode 1))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :init (setq lsp-keymap-prefix "C-l")
  :hook((python-mode . lsp)
	(typescript-mode . lsp)
	(java-mode . lsp)
	(css-mode . lsp)
	(web-mode . lsp))

  :config
  ;; Poor performance with languages that don't provide formatter and have formatter setup (py-yapf)
  ;;(add-hook 'before-save-hook 'lsp-format-buffer)

  (use-package lsp-java
    :ensure t
    :after lsp-mode)

  (use-package lsp-ui
    :ensure t
    :after lsp-mode
    :config (setq lsp-ui-doc-enable t
		  lsp-ui-peek-enable t
		  lsp-ui-sideline-enable nil
		  lsp-ui-doc-position (quote at-point))))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package csharp-mode
  :defer t
  :mode ("\\.cs?\\'" . csharp-mode)
  :config
  (setq indent-tabs-mode nil)

  (use-package omnisharp
    :ensure t
    :bind (("C-l r t" . omnisharp-unit-test-buffer)
	   ("C-l s s" . omnisharp-start-omnisharp-server)
	   ("C-l s q" . omnisharp-stop-server))

    :hook (csharp-mode . omnisharp-mode)
    :config
    (add-hook 'before-save-hook
	      (lambda ()
		(when (eq major-mode 'csharp-mode)
		  (omnisharp-code-format-entire-file))))))

(use-package python
  :defer t
  :config
  (setq python-shell-interpreter "python3")

  (use-package python-pytest
    :ensure t
    :bind ("C-c t" . python-pytest-popup))

  (use-package virtualenvwrapper
    :ensure t
    :config
    (venv-initialize-interactive-shells)
    (setq venv-location "~/.venv/"))

  (use-package flycheck-pyflakes
    :ensure t
    :after flycheck))

(use-package clojure-mode
  :defer t
  :config
  (use-package clj-refactor
    :ensure t
    :config (cljr-add-keybindings-with-prefix "C-c C-a"))
  (use-package cider
    :bind ("C-l" . cider-repl-clear-buffer)
    :config
    (setq cider-show-error-buffer 'only-in-repl)))

(use-package lispy
  :disabled
  :defer t
  :after clojure
  :config
  (add-hook 'clojure-mode-hook 'lispy-mode)
  (add-hook 'clojurescript-mode-hook 'lispy-mode))

(use-package paredit
  :disabled
  :defer t
  :after clojure
  :config
  ;; TODO: evil-paredit
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'clojurescript-mode-hook 'paredit-mode))

(use-package c-mode
  :mode (("\\.c?\\'" . c-mode)
	 ("\\.h?\\'" . c-mode)))

(use-package css-mode
  :defer t
  :mode (("\\.css\\'" . css-mode)))

(use-package web-mode
  :defer t
  :mode (("\\.html\\'" . web-mode)
	 ("\\.php\\'" . web-mode)
	 ("\\.ts?\\'" . web-mode)
	 ("\\.js?\\'" . web-mode)
	 ("\\.jsx?\\'" . web-mode)
	 ("\\.tsx?\\'" . web-mode))
  :config
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t)

  (use-package emmet-mode
    :ensure t
    :config
    :hook (web-mode . emmet-mode)))

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

(use-package google-this
  :ensure t
  :config
  (google-this-mode 1))

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

(use-package rainbow-delimiters
  :ensure
  :hook ((js-mode . rainbow-delimiters-mode)
	 (emacs-lisp-mode . rainbow-delimiters-mode)
	 (csharp-mode . rainbow-delimiters-mode)
	 (clojure-mode . rainbow-delimiters-mode)
	 (python-mode . rainbow-delimiters-mode)))

(use-package paren
  :config
  (setq show-paren-style 'expression)
  (setq show-paren-when-point-in-periphery t)
  (setq show-paren-when-point-inside-paren nil)
  :hook (after-init-hook . show-paren-mode))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-molokai t))
  ;;(load-theme 'doom-one-light t))

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

;;(global-hl-line-mode)
;;; End of GUI

;; Remove splash screen
(setq inhibit-splash-screen t)

;; Hide macOS top bar
(setq ns-auto-hide-menu-bar t)

;; Set color of macOS bar
(add-to-list 'default-frame-alist '(ns-appearance . light))

;; line numbers
(setq linum-format "%4d  ")
(global-linum-mode t)

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

;; Turn off creation of temp files
(setq make-backup-files nil)
(setq auto-save-default nil)

(add-to-list 'display-buffer-alist
	     `(,(regexp-quote "*shell") display-buffer-same-window))

;; Setup indentation
(setq indent-tabs-mode t)

;; Show tabs
(setq whitespace-style '(face tabs tab-mark trailing))

;;; End of Built-in Emacs

;;; Binds
;; German keyboard binding, by making emasc ignore right-alt key
(setq ns-right-alternate-modifier nil)

(global-set-key (kbd "C-x j") 'xref-find-definitions)
(global-set-key (kbd "C-x p") 'xref-pop-marker-stack)
(global-set-key (kbd "C-c b") 'shell)
(global-set-key (kbd "C-x C-b") 'ibuffer)
;;; End of Binds

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-process-echoes t)
 '(custom-safe-themes
   '("830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "5d09b4ad5649fea40249dd937eaaa8f8a229db1cec9a1a0ef0de3ccf63523014" "7b3d184d2955990e4df1162aeff6bfb4e1c3e822368f0359e15e2974235d9fa8" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "b89ae2d35d2e18e4286c8be8aaecb41022c1a306070f64a66fd114310ade88aa" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "bc4c89a7b91cfbd3e28b2a8e9e6750079a985237b960384f158515d32c7f0490" "c968804189e0fc963c641f5c9ad64bca431d41af2fb7e1d01a2a6666376f819c" "264b639ee1d01cd81f6ab49a63b6354d902c7f7ed17ecf6e8c2bd5eb6d8ca09c" "7bef2d39bac784626f1635bd83693fae091f04ccac6b362e0405abf16a32230c" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "196df8815910c1a3422b5f7c1f45a72edfa851f6a1d672b7b727d9551bb7c7ba" "aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" "bbb521edff9940ba05aeeb49f9b247e95e1cb03bd78de18122f13500bda6514f" "b8929cff63ffc759e436b0f0575d15a8ad7658932f4b2c99415f3dde09b32e97" "5a39d2a29906ab273f7900a2ae843e9aa29ed5d205873e1199af4c9ec921aaab" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "f9cae16fd084c64bf0a9de797ef9caedc9ff4d463dd0288c30a3f89ecf36ca7e" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "dde8c620311ea241c0b490af8e6f570fdd3b941d7bc209e55cd87884eb733b0e" "76f66cbdf9ada1f86f9225c0f33ae60b40d04073146c4f5c49d8189d1157728b" "3e3a1caddeee4a73789ff10ba90b8394f4cd3f3788892577d7ded188e05d78f4" "845103fcb9b091b0958171653a4413ccfad35552bc39697d448941bcbe5a660d" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "1526aeed166165811eefd9a6f9176061ec3d121ba39500af2048073bea80911e" "a339f231e63aab2a17740e5b3965469e8c0b85eccdfb1f9dbd58a30bdad8562b" "cb96a06ed8f47b07c014e8637bd0fd0e6c555364171504680ac41930cfe5e11e" "51956e440cec75ba7e4cff6c79f4f8c884a50b220e78e5e05145386f5b381f7b" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "d5f8099d98174116cba9912fe2a0c3196a7cd405d12fa6b9375c55fc510988b5" "7f791f743870983b9bb90c8285e1e0ba1bf1ea6e9c9a02c60335899ba20f3c94" "0ad7f1c71fd0289f7549f0454c9b12005eddf9b76b7ead32a24d9cb1d16cbcbd" "3577ee091e1d318c49889574a31175970472f6f182a9789f1a3e9e4513641d86" "a77ced882e25028e994d168a612c763a4feb8c4ab67c5ff48688654d0264370c" default))
 '(expand-region-preferred-python-mode 'fgallina-python)
 '(lsp-diagnostics-provider :flycheck)
 '(lsp-prefer-flymake nil)
 '(lsp-ui-doc-include-signature nil)
 '(lsp-ui-doc-position 'at-point)
 '(org-tags-column 80)
 '(package-selected-packages
   '(haskell-mode org-ref org counsel-spotify evil-paredit hl-todo gitignore-mode gitattributes-mode gitconfig-mode company-restclient json-reformat scala-mode yaml-mode gradle-mode protobuf-mode editorconfig darkroom lispy evil-collection smex python-pytest ng2-mode typescript-mode eyebrowse counsel-css clj-refactor counsel ivy which-key gruvbox-theme solarized-theme vterm skeletor lsp-java js-codemod restclient-helm omnisharp csharp-mode org-pdftools cider clojure-mode-extra-font-locking clojure-mode electric-pair-mode electric-pair rainbow-delimiter-mode rainbow-delimiters docker base16-theme color-theme-sanityinc-tomorrow mark-multiple lsp-ui js2-mode ace-jump-mode expand-region diff-hl omnisharp-mode prettier-js js-comint soothe-theme helm-lsp virtualenvwrapper ace-window py-yapf magit-popup spotify yasnippet flycheck-pyflake flycheck-pyflakes pyvenv web-mode web wanderlust use-package switch-window sublime-themes restclient rainbow-mode plantuml-mode php-mode peep-dired origami org-pdfview magit-todos key-chord htmlize google-this golden-ratio flymd exec-path-from-shell evil-surround evil-org evil-numbers evil-multiedit emmet-mode elfeed-org doom-themes company-web autopair))
 '(python-shell-interpreter "python3")
 '(show-paren-when-point-inside-paren t))
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 2.0))))
 '(show-paren-match ((t nil)))
 '(show-paren-match-expression ((t (:background "gray14")))))
(put 'upcase-region 'disabled nil)
