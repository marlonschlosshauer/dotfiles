(defun binds()
  (global-set-key (kbd "C-x j") 'xref-find-definitions)
  (global-set-key (kbd "C-x p") 'xref-pop-marker-stack)
  (global-set-key (kbd "C-x C-u") 'list-tags)

  ;; dired
  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd ".") 'dired-up-directory)

  ;; magit
  (global-set-key (kbd "C-x C-g") 'magit)

  ;; projectile
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  ;; Set org-capture keybinding
  (global-set-key (kbd "C-c c") 'org-capture)

  ;; Set better commenting bind
  (global-set-key (kbd "C-x c") 'comment-region)
  (global-set-key (kbd "C-x t") 'delete-trailing-whitespace)

  ;; Enable selecting windows by number
  (global-set-key (kbd "C-x o") 'switch-window)

  ;; Inc/Dec keys from vi
  (define-key evil-normal-state-map (kbd "+") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "-") 'evil-numbers/dec-at-pt)

  ;; Remap jump to start/end of line
  (define-key evil-normal-state-map (kbd "B") 'evil-beginning-of-visual-line)
  (define-key evil-normal-state-map (kbd "E") 'evil-end-of-visual-line)
  ;; TODO: Add smart jupm to start

  ;; Open rss entries in browser
  (evil-collection-define-key 'normal 'elfeed-search-mode-map
    (kbd "RET") 'elfeed-search-browse-url)

  (enable-german-binds))

(defun enable-german-binds()
  (global-set-key (kbd "M-5") '(lambda () (interactive) (insert "[")))
  (global-set-key (kbd "M-6") '(lambda () (interactive) (insert "]")))
  (global-set-key (kbd "M-7") '(lambda () (interactive) (insert "|")))
  (global-set-key (kbd "M-8") '(lambda () (interactive) (insert "{")))
  (global-set-key (kbd "M-9") '(lambda () (interactive) (insert "}")))
  (global-set-key (kbd "M-l") '(lambda () (interactive) (insert "@"))))

