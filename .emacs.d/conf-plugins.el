(defun conf-plugins ()
  ;; Setup evil
  (global-evil-surround-mode 1)

  (when (require 'evil-collection nil t)
	(evil-collection-init))
  (evil-mode 1)

  (setq evil-want-integration t)

  (add-hook 'after-init-hook 'global-company-mode)
  (global-undo-tree-mode)
  (yas-global-mode 1)

  ;; Add environment load paths
  (when (memq window-system '(mac ns x))
	(exec-path-from-shell-initialize))

  ;; Org-mode
  (setq org-return-follows-link t)

  ;; org-pdfview
  (eval-after-load 'org '(require 'org-pdfview))

  ;; Force org to open files in dired, instead of finder
  (add-to-list 'org-file-apps '(directory . emacs))

  ;; Agenda
  (setq org-agenda-files '((sequence "~/org/agenda.org")))
  (setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE" "CANCEL")))
  (setq org-default-notes-file "~/org/agenda.org")

  ;; evil-org
  (add-hook 'org-mode-hook 'evil-org-mode)
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  (evil-org-agenda-set-keys)

  (projectile-mode +1)

  ;; Make magit open in the current buffer
(setq magit-display-buffer-function
		(lambda (buffer)
		  (display-buffer
		   buffer (if (and (derived-mode-p 'magit-mode)
						   (memq (with-current-buffer buffer major-mode)
								 '(magit-process-mode
								   magit-revision-mode
								   magit-diff-mode
								   maigt-stash-mode
								   magit-status-mode)))
					  nil
					'(display-buffer-same-window)))))

  ;; Load possible yasnippets
  (yas-reload-all)

  ;; Turn on emmet
  (add-hook 'web-mode-hook  'emmet-mode)
  (add-hook 'php-mode-hook  'emmet-mode)

  ;; Handle lsp mode
  (add-hook 'web-mode-hook #'lsp)
  (add-hook 'typescript-mode-hook #'lsp)
  (add-hook 'csharp-mode-hook 'omnisharp-mode)

  ;; Start web-mode when in these files
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))

  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

  ;; Set highlighting
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t)

  ;; Enable company for web-mode
  (defun my-web-mode-hook ()
	(set (make-local-variable 'company-backends) '(company-css company-web-html company-yasnippet company-files)))

  (push 'company-lsp company-backends)
  (add-hook 'web-mode-hook 'my-web-mode-hook)

  ;; Start autopair
  (autopair-global-mode)

  ;; Start function folding
  (add-hook 'web-mode-hook 'origami-mode)

  ;; Start google-this
  (google-this-mode 1)

  ;; Map ESC to jj
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)

  ;; Deleted repeating blank lines
  (add-hook 'before-save-hook 'whitespace-cleanup)
  (add-hook 'before-save-hook 'xah-clean-whitespace)

  ;; pdf-tools
  ;(pdf-tools-install)
  (pdf-loader-install)
  (add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))

  ;; elfeed
  (setq-default elfeed-search-filter "@2-weeks-ago ")
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/org/rss.org"))

  ;; SQL Mode stuff
  (add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

  ;; PlantUML
  (setq org-plantuml-jar-path "~/plantuml.jar")
  (setq plantuml-jar-path "~/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar)

  (setq org-plantuml-executable-path "/usr/local/bin/plantuml")
  (setq plantuml-executable-path "/usr/local/bin/plantuml")
  (setq plantuml-default-exec-mode 'executable)

  (add-to-list
   'org-src-lang-modes '("plantuml" . plantuml))

  (with-eval-after-load 'org
	(org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

  ;; Change flymd browser from chrome to firefox
  (defun my-flymd-browser-function (url)
	(let ((process-environment (browse-url-process-environment)))
	  (apply 'start-process
			 (concat "firefox " url)
			 nil
			 "/usr/bin/open"
			 (list "-a" "firefox" url))))
  (setq flymd-browser-open-function 'my-flymd-browser-function))
