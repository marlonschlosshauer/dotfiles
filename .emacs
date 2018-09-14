;Removing GUI items
(menu-bar-mode -1)
(tool-bar-mode -1)

;Install EVIL
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

;Install undo-tree
(global-undo-tree-mode)

;Install goto-chg
(require 'goto-chg)
(global-set-key [(control ?.)] 'goto-last-change)
(global-set-key [(control ?,)] 'goto-last-change-reverse)
