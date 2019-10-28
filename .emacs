(load "~/.emacs.d/manage-plugins.el")
(start-manager)
; Do on initial install
(check-plugins)
(manage-plugins)

(load "~/.emacs.d/graphics.el")
(graphics)

(load "~/.emacs.d/conf-plugins.el")
(conf-plugins)

(load "~/.emacs.d/conf-emacs.el")
(conf-emacs)

(load "~/.emacs.d/binds.el")
(binds)

(load "~/.emacs.d/expiremental.el")
(load "~/.emacs.d/manage-functions.el")

;; Lazy fix for #34341 Bad Request
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; I hate how this is here
;; Package manager added this, no-touch
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/org/todo.org")))
 '(package-selected-packages
   (quote
	(restclient elfeed-org htmlize omnisharp magit-todos org-pdfview org-listcruncher pdf-tools wanderlust lsp-mode evil-org flycheck yasnippet-snippets typescript-mode csharp-mode projectile switch-window evil org ## helm flymd doom-themes sublime-themes php-mode key-chord google-this origami autopair yasnippet web-mode web golden-ratio exec-path-from-shell evil-surround evil-magit evil-collection emmet-mode company-web badwolf-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-tab ((t (:foreground "#636363")))))

  ;; Load path
  (when (memq window-system '(mac ns x))
	(exec-path-from-shell-initialize))

