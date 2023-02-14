;; Setup org agenda files
(setq org-agenda-files '("~/Library/CloudStorage/Dropbox/org/todo.org"
			 "~/Library/CloudStorage/Dropbox/gedankenessen/todo.org"
			 "~/Library/CloudStorage/Dropbox/neo/todo.org"
			 "~/Library/CloudStorage/Dropbox/TP/todo.org"))

;; Make "todo" the default org file
(setq org-default-notes-file "~/Library/CloudStorage/Dropbox/org/todo.org")

;; Add capture templates
(setq org-capture-templates
      '(("t" "TODO" entry (file+headline "~/Library/CloudStorage/Dropbox/org/todo.org" "Todo")
	 "* TODO %? %i\n  %a")
	("p" "TP" entry (file+headline "~/Library/CloudStorage/Dropbox/TP/todo.org" "Todo")
	 "* TODO %? \n %U")
	("g" "gde" entry (file+headline "~/Library/CloudStorage/Dropbox/gedankenessen/todo.org" "Todo")
	 "* TODO %? \n %U")
	("n" "neo" entry (file+headline "~/Library/CloudStorage/Dropbox/neo/todo.org" "Todo")
	 "* TODO %? %i\n  %a")))
