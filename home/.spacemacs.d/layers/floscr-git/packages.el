(setq floscr-git-packages
      '(
        magit
        ))

(defun floscr-git/post-init-magit ()
  (use-package magit
    :defer t
    :config
    (progn
      (setq magit-save-some-buffers nil)
      (setq magit-save-repository-buffers nil)
      (setq magit-display-buffer-function
            (lambda (buffer)
              (display-buffer
               buffer (if (not (memq (with-current-buffer buffer major-mode)
                                     '(magit-process-mode
                                       magit-revision-mode
                                       magit-diff-mode
                                       magit-stash-mode)))
                          '(display-buffer-same-window)
                        nil))))

      ;; automatically refresh when saving a tracked file
      (add-hook 'after-save-hook 'magit-after-save-refresh-status)
      )))
