(defconst floscr-defaults-packages
  '(
     neotree
     default-text-scale
     ))

(defun floscr-defaults/kill-window-or-buffer ()
  (interactive)
  (condition-case nil (delete-window) (error (kill-current-buffer)))
  )

(defun floscr-defaults/post-init-neotree ()
  (use-package neotree
    :config
    (evilified-state-evilify-map neotree-mode-map
      :mode neotree-mode
      :bindings
      (kbd "TAB") 'neotree-quick-look)
    )
  )

(defun floscr-defaults/init-default-text-scale ()
  (use-package default-text-scale
    :bind (
           ("C-M-=" . default-text-scale-increase)
           ("C-M--" . default-text-scale-decrease)
           ("C-M-0" . default-text-scale-reset)
           )))
