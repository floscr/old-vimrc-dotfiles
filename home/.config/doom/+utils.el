;;; ~/.homesick/repos/Dotfiles/home/.config/doom/+utils.el -*- lexical-binding: t; -*-

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (when (and (configuration-layer/package-usedp 'projectile)
                   (projectile-project-p))
          (call-interactively #'projectile-invalidate-cache))
        (message "File '%s' successfully removed" filename)))))

;; (defun is-term-buffer (b)
;;   "Check if buffer a buffer name matches doom terminal"
;;   (string-match-p "^\\*doom terminal" (buffer-name b)))

;; (defun +term/popup-new-or-existing-term ()
;;   "Pops up a new or an existing term buffer"
;;   (interactive)
;;   (let ((persp-term-buffer (cl-find-if #'is-term-buffer (persp-buffer-list))))
;;     (if persp-term-buffer
;;         (pop-to-buffer (save-window-excursion persp-term-buffer))
;;       (+term/open-popup-in-project)))
;;   )

;; (defun +term/popup-new-or-existing-term ()
;;   "Pops up a new or an existing term buffer"
;;   (interactive)
;;   (let ((popup-term (cl-find-if #'is-term-buffer (+popup-windows) :key #'window-buffer)))
;;     (if popup-term
;;         (+popup/close popup-term)
;;       (+term/open-popup-in-project)))
;;   )

;; (+term/popup-new-or-existing-term)

;; (+term/popup-new-or-existing-term)
