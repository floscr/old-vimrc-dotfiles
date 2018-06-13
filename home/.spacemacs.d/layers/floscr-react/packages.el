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

(eval-after-load 'react-mode
  '(add-hook 'react-mode-hook #'add-node-modules-path))

(add-hook 'react-mode-hook 'eslintd-fix-mode)

(defun floscr-react/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (push 'javascript-jshint flycheck-disabled-checkers)
    (push 'json-jsonlint flycheck-disabled-checkers))
  (spacemacs/enable-flycheck 'react-mode))

(defun floscr-react/init-react-mode ()
  (use-package react-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
      (add-to-list 'auto-mode-alist '("\\.react.js\\'" . rjsx-mode))
      (add-to-list 'auto-mode-alist '("\\index.android.js\\'" . rjsx-mode))
      (add-to-list 'auto-mode-alist '("\\index.ios.js\\'" . rjsx-mode))
      (add-to-list 'magic-mode-alist '("/\\*\\* @jsx React\\.DOM \\*/" . rjsx-mode))
      (add-to-list 'magic-mode-alist '("^import React" . rjsx-mode)))
    ))
