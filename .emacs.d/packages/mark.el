;;; mark.el --- My custom package

(defun mark-before-call	(orig-fn &rest args)
	(push-mark)
	(apply orig-fn args))

(provide 'mark)

;;; mark.el ends here
