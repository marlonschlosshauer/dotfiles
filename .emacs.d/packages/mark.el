;;; mark.el --- Add mark before calling -*- lexical-binding: t; -*-

;;; Commentary:

;; Mark, then call.  Make money.

;;; Code:
(defun mark-before-call (fn &rest args)
  "Push the current point to the global mark ring before calling FN with ARGS."
  (push-mark (point) t)
  (apply fn args))

(provide 'mark)

;;; mark.el ends here
