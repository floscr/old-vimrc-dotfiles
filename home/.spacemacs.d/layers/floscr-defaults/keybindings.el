(when (spacemacs/system-is-mac)
  (global-set-key (kbd "H-h") 'ns-do-hide-emacs)
  (global-set-key (kbd "H-w") 'floscr-defaults/kill-window-or-buffer)
  (global-set-key (kbd "H-M-h") 'ns-do-hide-others)
  (global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
  (global-set-key (kbd "<H-return>") 'toggle-frame-fullscreen)
  )

(define-key global-map (kbd "C-h") #'evil-window-left)
(define-key global-map (kbd "C-j") #'evil-window-down)
(define-key global-map (kbd "C-k") #'evil-window-up)
(define-key global-map (kbd "C-l") #'evil-window-right)

(evil-define-key 'normal 'global "gn" 'tabbar-forward-tab)
(evil-define-key 'normal 'global "gp" 'tabbar-backward-tab)
