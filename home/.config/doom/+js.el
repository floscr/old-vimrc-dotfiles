;;; ~/.homesick/repos/Dotfiles/home/.config/doom/+js.el -*- lexical-binding: t; -*-

(setq
 flycheck-javascript-eslint-executable (executable-find "eslint_d")
 flycheck-disabled-checkers '(javascript-jshint javascript))

(after! rjsx-mode (add-hook 'js2-mode-hook #'eslintd-fix-mode))
(after! js2-mode (add-hook 'js2-mode-hook #'eslintd-fix-mode))

(defun remove-js-ext (f)
  "Remove js extension from string"
  (replace-regexp-in-string "\.js$" "" f))

(defun buffer-file-name-relative ()
  "Extranct the filename with extension from path"
  (replace-regexp-in-string (file-name-directory buffer-file-name) "" (buffer-file-name)))

(defun match-const-function-name (line)
  "Matches a line to the word after the declaration"
  (nth 2 (s-match
          "\\(const\\|let\\|class\\)\s\\(.+?\\)\s"
          line)))

(defun const-function-at-point ()
  "Returns the current function name at the current line"
  (match-const-function-name (thing-at-point 'line t)))

(defun js2r-export-default ()
  "Exports the current declaration at the end of the file"
  (interactive)
  (save-excursion
    (let* ((name (const-function-at-point)))
      (goto-char (point-max))
      (insert "\n")
      (insert (template "export default <<name>>;")))))

(defun js2r-extract-const-to-file ()
  "Extracts function to external file"
  (interactive)
  (let* ((name (const-function-at-point))
         (path (concat "./" name ".js")))
    (evil-digit-argument-or-evil-beginning-of-line)
    (js2r-kill)
    (f-write-text "" 'utf-8 path)
    (find-file path)
    (yank)))

(defun js-index-file-names ()
  "Get filenames from current buffers directory"
  (let ((fs (directory-files default-directory nil ".*\\.js")))
    (mapcar 'remove-js-ext
            (remove (buffer-file-name-relative) fs))))

(defun +js|generate-index ()
  "Generate an index import file for files in directory"
  (interactive)
  (erase-buffer)
  (let* ((fs (js-index-file-names)))
    (mapc (lambda (f) (insert "import " f " from './" f "';\n")) fs)
    (insert "\n")
    (insert "export default {\n")
    (mapc (lambda (f) (insert "    " f ",\n")) fs)
    (insert "};")))

(defun js2r-sexp-to-template-string ()
  "Wrap sexp into a template string"
  (interactive)
  (kill-sexp)
  (insert (concat "`${" (substring-no-properties (car kill-ring)) "}`"))
  (pop kill-ring))

(defun +js|load-evil-camel-case-motion ()
  (require 'evil-little-word)
  (define-key evil-normal-state-map (kbd "A-w") 'evil-forward-little-word-begin)
  (define-key evil-normal-state-map (kbd "A-b") 'evil-backward-little-word-begin)
  (define-key evil-operator-state-map (kbd "A-w") 'evil-forward-little-word-begin)
  (define-key evil-operator-state-map (kbd "A-b") 'evil-backward-little-word-begin)
  (define-key evil-visual-state-map (kbd "A-w") 'evil-forward-little-word-begin)
  (define-key evil-visual-state-map (kbd "A-b") 'evil-backward-little-word-begin)
  (define-key evil-visual-state-map (kbd "i A-w") 'evil-inner-little-word))

(after! rjsx-mode
  (+js|load-evil-camel-case-motion))

(after! js2-mode
  (+js|load-evil-camel-case-motion))
