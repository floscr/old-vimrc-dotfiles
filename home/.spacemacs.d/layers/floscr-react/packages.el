(defconst floscr-react-packages
  '(
    add-node-modules-path
    flycheck
    eslintd-fix
    evil-matchit
    react-mode
    ))

(defun floscr-react/post-init-add-node-modules-path ()
  (with-eval-after-load 'react-mode
    (add-hook 'react-mode-hook #'add-node-modules-path)))

(add-hook 'react-mode-hook 'eslintd-fix-mode)

(defun floscr-react/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (push 'javascript-jshint flycheck-disabled-checkers)
    (push 'json-jsonlint flycheck-disabled-checkers))
  (spacemacs/enable-flycheck 'react-mode))
