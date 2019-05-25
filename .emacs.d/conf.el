(defun conf()
  ;; Set enviroment variables
  (setenv "PATH"
		  (concat
		   "/usr/local/bin" ";"
		   "/usr/bin" ";"
		   "/bin" ";"
		   "/usr/sbin" ";"
		   "/sbin" ";"
		   (getenv "PATH")
		   )
		  )

  ;; Change font color for keywords
  (if (fboundp 'global-font-lock-mode)
	  (global-font-lock-mode 1))

  ;; Turn auto reload of buffer (on file change) on
  (global-auto-revert-mode t)

  ;; Enable line wrapping
  (add-hook 'org-mode-hook 'auto-fill-mode)

  ;; Set line wrap
  (setq-default fill-column 80)

  ;; Org-mode
  (setq org-agenda-files '("~/Projects/org/todo.org"))
  (setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "DEL")))
  (setq org-capture-templates
		'(
		  ("t" "TODO " entry
		   (file "~/Projects/org/todo.org")
		   "* TODO %?
SCHEDULED: %t")

		  ("s" "SCHEDULE " entry
		   (file "~/Projects/org/schedule.org")
		   "* SCHEDULE %?
SCHEDULED: %t")

		  ("r" "REMINDER " entry
		   (file "~/Projects/org/reminder.org")
		   "* REMINDER %?
SCHEDULED: %t")))

  ;; Turn off creation of temp files
  (setq make-backup-files nil)
  (setq auto-save-default nil)

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
  ;;Enable whitespace mode everywhere
  (global-whitespace-mode)

  (put 'upcase-region 'disabled nil)
  (put 'erase-buffer 'disabled nil))
