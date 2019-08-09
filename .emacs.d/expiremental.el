(defun build (target)
  (interactive
	 (compile (concat "make " target))))
