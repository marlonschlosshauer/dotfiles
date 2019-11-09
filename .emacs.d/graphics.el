(defun graphics ()
  ;; Load theme
  (load-theme 'doom-one t)

  ;; Removing GUI items
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  ;; Remove splash screen
  (setq inhibit-splash-screen t)

  ;; Hide macOS top bar
  (setq ns-auto-hide-menu-bar t)

  ;; line numbers
  (setq linum-format "%4d \u2502 ")
  (global-linum-mode t)

  ;; Highlight closing tags (parenthesis, brackets)
  (show-paren-mode t)
  (setq show-paren-delay 0)

  ;; Make top bar light
  (add-to-list 'default-frame-alist '(ns-appearance . light)))

