(defconst my-javascript-packages
  '(add-node-modules-path
    flycheck
    rjsx-mode))

(defun itome-react/post-init-add-node-modules-path ()
  (with-eval-after-load 'rjsx-mode
    (add-hook 'rjsx-mode-hook #'add-node-modules-path)))

(defun itome-react/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (push 'javascript-jshint flycheck-disabled-checkers)
    (push 'json-jsonlint flycheck-disabled-checkers))
  (spacemacs/enable-flycheck 'rjsx-mode))
;; (spacemacs/add-flycheck-hook 'rjsx-mode))

(setq itome-react-packages
      '(
        rjsx-mode
        ;; emmet-mode
        evil-matchit
        rjsx-mode
        ))

(defun itome-react/init-rjsx-mode ()
  (use-package rjsx-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
      (add-to-list 'auto-mode-alist '("\\.react.js\\'" . rjsx-mode))
      (add-to-list 'auto-mode-alist '("\\index.android.js\\'" . rjsx-mode))
      (add-to-list 'auto-mode-alist '("\\index.ios.js\\'" . rjsx-mode))
      (add-to-list 'magic-mode-alist '("/\\*\\* @jsx React\\.DOM \\*/" . rjsx-mode))
      (add-to-list 'magic-mode-alist '("^import React" . rjsx-mode)))
    :config
    ;; (progn
    ;;   ;; prefixes
    ;;   (spacemacs/declare-prefix-for-mode 'rjsx-mode "mh" "documentation")
    ;;   (spacemacs/declare-prefix-for-mode 'rjsx-mode "mr" "refactor")
    ;;   (spacemacs/declare-prefix-for-mode 'rjsx-mode "mg" "goto")
    ;;   (spacemacs/declare-prefix-for-mode 'rjsx-mode "mz" "folding")
    ;;   ;; key bindings
    ;;   (evil-define-key 'insert rjsx-mode-map (kbd "C-d") 'spacemacs//rjsx-delete-creates-full-tag-with-insert)
    ;;   (evil-define-key 'normal rjsx-mode-map (kbd "C-d") 'spacemacs//rjsx-delete-creates-full-tag-with-insert)
    ;;   (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode
    ;;     "w" 'js2-mode-toggle-warnings-and-errors
    ;;     "zc" 'js2-mode-hide-element
    ;;     "zo" 'js2-mode-show-element
    ;;     "zr" 'js2-mode-show-all
    ;;     "ze" 'js2-mode-toggle-element
    ;;     "zF" 'js2-mode-toggle-hide-functions
    ;;     "zC" 'js2-mode-toggle-hide-comments))
    ))

