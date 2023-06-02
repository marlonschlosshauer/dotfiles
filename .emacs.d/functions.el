(defun get-authinfo-value (host user &optional port)
  "Retrieve a value from the Authinfo file for the specified host, user, and port."
  (let ((match (auth-source-search :host host :user user :port port)))
    (when match
      (let ((secret (plist-get (car match) :secret)))
        (when secret
          (if (functionp secret)
              (funcall secret)
            secret))))))
