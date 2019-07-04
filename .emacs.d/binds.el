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

  (enable-german-binds)
)

(defun enable-german-binds()
 ;; Meta-5
  (global-set-key (kbd "M-5") '(lambda () (interactive) (insert "[")))
  ;; Meta-6
  (global-set-key (kbd "M-6") '(lambda () (interactive) (insert "]")))
  ;; Meta-8
  (global-set-key (kbd "M-8") '(lambda () (interactive) (insert "{")))
  ;; Meta-9
  (global-set-key (kbd "M-9") '(lambda () (interactive) (insert "}")))
  ;; Meta-l
  (global-set-key (kbd "M-l") '(lambda () (interactive) (insert "@"))))
