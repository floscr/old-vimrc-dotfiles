(defun floscr-git/magit-stage-and-commit-current-buffer ()
  "Save, stage and commit the current buffer"
  (interactive)
  (save-buffer)
  (magit-stage-file buffer-file-name)
  (magit-commit)
  )
