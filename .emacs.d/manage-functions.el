(defun xah-clean-whitespace ()
  "Delete trailing whitespace, and replace repeated blank lines to just 1.
Only space and tab is considered whitespace here.
Works on whole buffer or text selection, respects `narrow-to-region'.

URL `http://ergoemacs.org/emacs/elisp_compact_empty_lines.html'
Version 2017-09-22"
  (interactive)
  (let ($begin $end)
	(if (region-active-p)
		(setq $begin (region-beginning) $end (region-end))
	  (setq $begin (point-min) $end (point-max)))
	(save-excursion
	  (save-restriction
		(narrow-to-region $begin $end)
		(progn
		  (goto-char (point-min))
		  (while (re-search-forward "[ \t]+\n" nil "move")
			(replace-match "\n")))
		(progn
		  (goto-char (point-min))
		  (while (re-search-forward "\n\n\n+" nil "move")
			(replace-match "\n\n")))
		(progn
		  (goto-char (point-max))
		  (while (equal (char-before) 32) ; char 32 is space
			(delete-char -1))))
	  (message "white space cleaned"))))

;;(defun open-elfeed-link-in-browser ()
;;  (interactive)
;;  (let ((entry) ((elfeed-search-selected :single)))
;;	()
;;)
