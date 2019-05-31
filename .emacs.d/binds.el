(defun binds()
  (global-set-key (kbd "C-x j") 'xref-find-definitions)
  (global-set-key (kbd "C-x p") 'xref-pop-marker-stack)
  (global-set-key (kbd "C-x C-u") 'list-tags)

  ;; Set org-capture keybinding
  (global-set-key (kbd "C-c c") 'org-capture)

  ;; Set better commenting bind
  (global-set-key (kbd "C-x c") 'comment-region)
  (global-set-key (kbd "C-x t") 'delete-trailing-whitespace)

  ;; Enable selecting windows by number
  (global-set-key (kbd "C-x o") 'switch-window)

  ;; Inc/Dec keys from vi
  (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)
)
