;;; ~/.homesick/repos/Dotfiles/home/.config/doom/+utils.el -*- lexical-binding: t; -*-

(defsubst curry (function &rest arguments)
  (lexical-let ((function function)
                (arguments arguments))
    (lambda (&rest more) (apply function (append arguments more)))))

(defsubst rcurry (function &rest arguments)
  (lexical-let ((function function)
                (arguments arguments))
    (lambda (&rest more) (apply function (append more arguments)))))

(defsubst compose (function &rest more-functions)
  (cl-reduce (lambda (f g)
               (lexical-let ((f f) (g g))
                 (lambda (&rest arguments)
                   (funcall f (apply g arguments)))))
             more-functions
             :initial-value function))

;;; compact display
(defun pretty-curry-compose ()
  (mapc (lambda (pair)
          (let ((regexp (car pair))
                (symbol (cdr pair)))
            (font-lock-add-keywords 'emacs-lisp-mode
              `((,regexp
                 (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                           ,symbol)
                           nil)))))))
        '(("(\\(compose\\)[ \t\n\r]" . ?\∘)
          ("(\\(curry\\)[ \t\n\r]" . ?\»)
          ("(\\(rcurry\\)[ \t\n\r]" . ?\«))))
(add-to-list 'emacs-lisp-mode-hook 'pretty-curry-compose)

;;; color these functions like keywords
(font-lock-add-keywords 'emacs-lisp-mode
                        '(("(\\(compose\\)[ \t\n\r]" 1 font-lock-keyword-face)
                          ("(\\(curry\\)[ \t\n\r]" 1 font-lock-keyword-face)
                          ("(\\(rcurry\\)[ \t\n\r]" 1 font-lock-keyword-face)))

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (when (and (configuration-layer/package-usedp 'projectile)
                   (projectile-project-p))
          (call-interactively #'projectile-invalidate-cache))
        (message "File '%s' successfully removed" filename)))))

(defun do-jxa-script (cmd)
  "Run a osx javascript automation script via bash"
  (shell-command-to-string
   (concat "osascript -l 'JavaScript' -e '" cmd "'")))

;; (defun is-term-buffer (b)
;;   "Check if buffer a buffer name matches doom terminal"
;;   (string-match-p "^\\*doom terminal" (buffer-name b)))

;; (defun +term/popup-new-or-existing-term ()
;;   "Pops up a new or an existing term buffer"
;;   (interactive)
;;   (let ((persp-term-buffer (cl-find-if #'is-term-buffer (persp-buffer-list))))
;;     (if persp-term-buffer
;;         (pop-to-buffer (save-window-excursion persp-term-buffer))
;;       (+term/open-popup-in-project)))
;;   )

;; (defun +term/popup-new-or-existing-term ()
;;   "Pops up a new or an existing term buffer"
;;   (interactive)
;;   (let ((popup-term (cl-find-if #'is-term-buffer (+popup-windows) :key #'window-buffer)))
;;     (if popup-term
;;         (+popup/close popup-term)
;;       (+term/open-popup-in-project)))
;;   )

;; (+term/popup-new-or-existing-term)

;; (+term/popup-new-or-existing-term)


(defun +doom|open-elisp-scratch-buffer ()
  "Opens a new scratch buffer in elisp mode"
  (interactive)
  (doom/open-scratch-buffer)
  (emacs-lisp-mode))
