(defconst floscr-defaults-packages
  '(neotree))

(defun floscr-defaults/post-init-neotree ()
  (use-package neotree
    :config
    (evilified-state-evilify-map neotree-mode-map
      :mode neotree-mode
      :bindings
      (kbd "TAB") 'neotree-quick-look)
    )
  )
