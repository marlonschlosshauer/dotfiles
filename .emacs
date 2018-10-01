;;Install Plugins
;MEPLA package manager stuff, I think.
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

;Install evil
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

;Install evil-surround
(require 'evil-surround)

;Install undo-tree
(global-undo-tree-mode)

;Install goto-chg
(require 'goto-chg)

;;Plugin setup
;Setup goto-chg (for omnisharp)
(global-set-key [(control ?.)] 'goto-last-change)
(global-set-key [(control ?,)] 'goto-last-change-reverse)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil-magit magit evil-surround badwolf-theme undo-tree omnisharp goto-chg company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;Enable company (and C# code completion)
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load
 'company
 '(add-to-list 'company-backends #'company-omnisharp))

;omnisharp configuration
(add-hook 'csharp-mode-hook 'omnisharp-mode)
(add-hook 'csharp-mode-hook #'company-mode)

;; Make evil available in packages
(with-eval-after-load 'evil
  ;; use evil mode in the buffer created from calling `list-packages'.
  (add-to-list 'evil-buffer-regexps '("*Packages*" . normal))

  (with-eval-after-load 'package
    ;; movement keys j,k,l,h set up for free by defaulting to normal mode.
    ;; mark, unmark, install
    (evil-define-key 'normal package-menu-mode-map (kbd "m") #'package-menu-mark-install)
    (evil-define-key 'normal package-menu-mode-map (kbd "u") #'package-menu-mark-unmark)
    (evil-define-key 'normal package-menu-mode-map (kbd "x") #'package-menu-execute))
  )

;Pull up magit
(global-set-key (kbd "C-x g") 'magit-status)

;;Graphical settings
;Load theme
(load-theme 'badwolf t)

;Removing GUI items
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;Change scrollbar color
(setq default-frame-alist '((scroll-bar-background . "black")))
(setq scroll-bar-background '((scroll-bar-background . "black")))
(set-face-background 'scroll-bar "black")

;Hide macOS top bar
(setq ns-auto-hide-menu-bar t)

;Show line numbers
(global-linum-mode t)

;;Change feel
(setq default-tab-width 4)
