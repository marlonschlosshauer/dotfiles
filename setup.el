(defun setup ()
;;; Setup plugins

  (add-hook 'after-init-hook 'global-company-mode)
  (global-undo-tree-mode)
  (yas-global-mode 1)
  (global-evil-surround-mode 1)

  (when (memq window-system '(mac ns x))
	(exec-path-from-shell-initialize))

  ;; Bind magit
  (global-set-key (kbd "C-x g") 'magit-status)

  ;; Load possible yasnippets
  (yas-reload-all)

  ;; Turn on emmet
  (add-hook 'web-mode-hook  'emmet-mode)

  ;; Start web-mode when in these files
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

  ;; Set highlighting
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t)

  ;; Enable company for web-mode
  (defun my-web-mode-hook ()
	(set (make-local-variable 'company-backends) '(company-css company-web-html company-yasnippet company-files))
	)

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

  ;; Change flymd browser from chrome to firefox
  (defun my-flymd-browser-function (url)
	(let ((process-environment (browse-url-process-environment)))
	  (apply 'start-process
			 (concat "firefox " url)
			 nil
			 "/usr/bin/open"
			 (list "-a" "firefox" url))))
  (setq flymd-browser-open-function 'my-flymd-browser-function))
