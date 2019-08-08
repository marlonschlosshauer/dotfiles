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

  ;; Show line numbers
  (global-linum-mode t)

  ;; Make top bar light
  (add-to-list 'default-frame-alist '(ns-appearance . light)))


