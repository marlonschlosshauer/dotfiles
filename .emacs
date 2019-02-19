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

;; Install company
(require 'company)
(require 'company-web-html)
(add-hook 'after-init-hook 'global-company-mode)
;;(define-key web-mode-map (kbd "C-;") 'company-web-html)

;; Install undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

;; Install goto-chg
(require 'goto-chg)

;; Install evil-surround
(require 'evil-surround)
(global-evil-surround-mode 1)

;; Install evil-magit
(require 'evil-magit)

;; Install yasnippets
(require 'yasnippet)
(yas-global-mode 1)

;;; Setup plugins 
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Bind magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Load possible yasnippets
(yas-reload-all)

;; Turn on emmet
(add-hook 'web-mode-hook  'emmet-mode)

;;; Setup web-mode

;; Start web-mode when in these files
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

;; Set highlighting
(setq web-mode-enable-current-column-highlight t)
(setq web-mode-enable-current-element-highlight t)

;; Enable company for web-mode
(defun my-web-mode-hook ()
  (set (make-local-variable 'company-backends) '(company-css company-web-html company-yasnippet company-files))
  )

;;; Configure emacs

;; Turn off creation of temp files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Change C Mode indenting to 4 spaces instead of 1 or what ever the stupid looking default is.
(setq-default c-basic-offset 4)

;; CC Mode sane indenting
(setq c-default-style "linux"
      c-basic-offset 4)
;; Package manager added this, no-touch
(custom-set-variables
 '(package-selected-packages
   (quote
    (yasnippet web-mode web omnisharp golden-ratio exec-path-from-shell evil-surround evil-magit evil-collection emmet-mode company-web badwolf-theme))))
(custom-set-faces
 )
