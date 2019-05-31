(load "~/.emacs.d/plugins.el")
(plugins)

(load "~/.emacs.d/graphics.el")
(graphics)

(load "~/.emacs.d/setup.el")
(setup)

(load "~/.emacs.d/conf.el")
(conf)

(load "~/.emacs.d/binds.el")
(binds)

(load "~/.emacs.d/custom.el")

;; Package manager added this, no-touch
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
	("~/Projects/org/todo.org" "/Users/akira/Projects/org/ProjectRA.org")))
 '(package-selected-packages
   (quote
	(projectile switch-window evil org ## helm flymd doom-themes sublime-themes php-mode key-chord google-this origami autopair yasnippet web-mode web golden-ratio exec-path-from-shell evil-surround evil-magit evil-collection emmet-mode company-web badwolf-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-tab ((t (:foreground "#636363")))))
