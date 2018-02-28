(define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)
(define-key helm-map (kbd "C-w") 'evil-delete-backward-word)

;; Buffer switching
(define-key evil-normal-state-map (kbd "gb") 'evil-switch-to-windows-last-buffer)
(define-key evil-normal-state-map (kbd "gn") 'next-buffer)
(define-key evil-normal-state-map (kbd "gp") 'previous-buffer)
