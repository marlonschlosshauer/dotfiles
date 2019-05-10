(load "~/plugins.el")
(plugins)

(load "~/graphics.el")
(graphics)

(load "~/setup.el")
(setup)

(load "~/conf.el")
(conf)

;; Package manager added this, no-touch
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(helm flymd doom-themes sublime-themes php-mode key-chord google-this origami autopair yasnippet web-mode web golden-ratio exec-path-from-shell evil-surround evil-magit evil-collection emmet-mode company-web badwolf-theme))))

(put 'upcase-region 'disabled nil)

(put 'erase-buffer 'disabled nil)
