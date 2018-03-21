(defun floscr-git/stage-and-commit-current-buffer ()
  "Stage and commit the current file"
  (interactive)
  (magit-stage-file)
  ;; (magit-commit)
  )
