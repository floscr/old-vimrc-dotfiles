;;; ~/.homesick/repos/Dotfiles/home/.config/doom/+js.el -*- lexical-binding: t; -*-

(setq
 flycheck-javascript-eslint-executable (executable-find "eslint_d")
 flycheck-disabled-checkers '(javascript-jshint javascript))

(after! rjsx-mode (add-hook 'js2-mode-hook #'eslintd-fix-mode))

(defun remove-js-ext (f)
  "Remove js extension from string"
  (replace-regexp-in-string "\.js$" "" f))

(defun buffer-file-name-relative ()
  "Extranct the filename with extension from path"
  (replace-regexp-in-string (file-name-directory buffer-file-name) "" (buffer-file-name)))

(defun js-index-file-names ()
  "Get filenames from current buffers directory"
  (let ((fs (directory-files default-directory nil ".*\\.js")))
    (mapcar 'remove-js-ext
            (remove (buffer-file-name-relative) fs))
    ))

(defun +js|generate-index ()
  "Generate an index import file for files in directory"
  (interactive)
  (erase-buffer)
  (let* ((fs (js-index-file-names)))
    (mapc (lambda (f) (insert "import " f " from './" f "';\n")) fs)
    (insert "\n")
    (insert "export default {\n")
    (mapc (lambda (f) (insert "    " f ",\n")) fs)
    (insert "};")
    ))

(defun +js|load-evil-camel-case-motion ()
  (require 'evil-little-word)
  (define-key evil-normal-state-map (kbd "w") 'evil-forward-little-word-begin)
  (define-key evil-normal-state-map (kbd "b") 'evil-backward-little-word-begin)
  (define-key evil-operator-state-map (kbd "w") 'evil-forward-little-word-begin)
  (define-key evil-operator-state-map (kbd "b") 'evil-backward-little-word-begin)
  (define-key evil-visual-state-map (kbd "w") 'evil-forward-little-word-begin)
  (define-key evil-visual-state-map (kbd "b") 'evil-backward-little-word-begin)
  (define-key evil-visual-state-map (kbd "i w") 'evil-inner-little-word))

(after! rjsx-mode
  (+js|load-evil-camel-case-motion))

(after! js2-mode
  (+js|load-evil-camel-case-motion))
