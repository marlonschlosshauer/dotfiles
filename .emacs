;;; Set enviroment variables
(setenv "PATH"
	(concat
	 "/Users/akira/Documents/templates/c#/" ";"
	 "/Users/akira/Documents/shellscripts" ";"
	 "/usr/local/bin" ";"
	 "/usr/bin" ";"
	 "/bin" ";"
	 "/usr/sbin" ";"
	 "/sbin" ";"
	 "/usr/local/lib/mono" ";" "~/.aspnet" ";"
	 "~/.dotnet" ";"
	 "~/.local/share/NuGet" ";"
	 "~/.mono" ";"
	 "~/.nuget" ";"
	 "~/.omnisharp" ";"
	 (getenv "PATH")
	 )
	)

;;; Install package manager
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;;; Graphical settings

;; Load theme
(load-theme 'badwolf t)

;; Removing GUI items
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Remove splash screen
(setq inhibit-splash-screen t)

;; Hide macOS top bar
(setq ns-auto-hide-menu-bar t)

;; Show line numbers
(global-linum-mode t)

;;; Install Plugins
;; Install evil
(add-to-list 'load-path "~/.emacs.d/evil")
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(require 'evil)
(when (require 'evil-collection nil t)
  (evil-collection-init))
(evil-mode 1)

(require 'company)
(require 'company-web-html)
(require 'undo-tree)
(require 'goto-chg)
(require 'evil-surround)
(require 'evil-magit)
(require 'yasnippet)
(require 'autopair)
(require 'whitespace)

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

;; Start autopair
(autopair-global-mode)

;; Start function folding
(add-hook 'web-mode-hook 'origami-mode)

;;; Configure emacs

;; Turn auto reload of buffer (on file change) on
(global-auto-revert-mode t)

;; Turn off creation of temp files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Set better commenting bind
(global-set-key (kbd "C-x c") 'comment-region)

;; Set font
(set-frame-font "SF Mono Medium" nil t)

;; Setup indentation
(setq indent-tabs-mode t)
(setq-default tab-width 4)
(setq c-default-style "linux")
(setq-local c-basic-offset 4)
(setq-local javascript-indent-level 4)
(setq-local js-indent-level 4)
(setq-local js4-basic-offset 4)
(setq-local web-mode-markup-indent-offset 4)
(setq-local web-mode-css-indent-offset 4)
(setq-local web-mode-code-indent-offset 4)
(setq-local css-indent-offset 4)

;; Show tabs
(setq whitespace-style '(face tabs tab-mark trailing))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-tab ((t (:foreground "#636363")))))

(setq whitespace-display-mappings
 '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'

(global-whitespace-mode) ; Enable whitespace mode everywhere


;; Package manager added this, no-touch
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(origami autopair yasnippet web-mode web golden-ratio exec-path-from-shell evil-surround evil-magit evil-collection emmet-mode company-web badwolf-theme))))

(put 'upcase-region 'disabled nil)

