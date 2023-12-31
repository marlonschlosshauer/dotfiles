(defun get-authinfo-value (host user &optional port)
  "Retrieve a value from the Authinfo file for the specified host, user, and port."
  (let ((match (auth-source-search :host host :user user :port port)))
    (when match
      (let ((secret (plist-get (car match) :secret)))
        (when secret
          (if (functionp secret)
              (funcall secret)
            secret))))))

(defun jira-open-ticket ()
  (interactive)
  (let ((project (read-string "Enter project: "))
        (code (read-string "Enter ticket: ")))
    (browse-url (concat "https://becklyn.atlassian.net/browse/" (upcase project) "-" code))))

(defun github-open-repo ()
  (interactive)
  (let ((repo (read-string "Enter repo: ")))
    (browse-url (concat "https://github.com/becklyn-studios/" repo))))

(defun px-to-region (px)
  (concat (number-to-string (/ (string-to-number (first (split-string px "px"))) 10.0 )) "rem"))

(defun px-to-rem-region ()
  (interactive)
  (when (region-active-p)
    (let ((text (buffer-substring (region-beginning) (region-end))))
      (delete-region (region-beginning) (region-end))
      (insert (px-to-region text)))))
