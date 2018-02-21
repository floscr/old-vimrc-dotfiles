(defconst
  floscr-packages
  '(
    evil-replace-with-register
    ))

(defun bkudria/init-evil-replace-with-register ()
  (use-package evil-extra-operator :defer t
    :init
    (define-key evil-motion-state-map "gr" 'evil-replace-with-register)))
