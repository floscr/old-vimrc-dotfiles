;;; languages/org/+my-agenda-fp.el -*- lexical-binding: t; -*-

(defun +agenda-files-to-buffers (files)
  (-map (lambda (f) (if (file-exists-p f)
                        (org-get-agenda-file-buffer f)
                      (error "No such file %s" f))) files))

(defun +agenda-file-to-entry-list (buffer)
  (with-current-buffer buffer))



(defun +agenda-files-to-entry-list (files))

(defun +agenda-tasks-fp ()
  (let* (file buffer
         (files (org-agenda-files nil 'ifmode))
         (buffers (+agenda-files-to-buffers files)))))
