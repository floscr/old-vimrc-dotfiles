;;; ~/.homesick/repos/Dotfiles/home/.config/doom/+utils.el -*- lexical-binding: t; -*-

(defun +flyspell|save-word ()
  "Save the current word to dictionary"
  (interactive)
  (let* ((current-location (point))
         (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

(defun +org-web-tools-dwim-at-point ()
  "Pass url to web tools from either:
1. An org link under the cursor
2. An url in the clipboard"
  (interactive)
  (let ((org-url (org-element-property :raw-link (org-element-context)))
        (clipboard-url (current-kill 0)))
    (if org-url
        (message "Reading org url from thing at point")
        (org-web-tools-read-url-as-org org-url)
      (if (string-match url-handler-regexp clipboard-url)
          (message "Reading org url from clipboard")
          (org-web-tools-read-url-as-org clipboard-url)
        (message "No url found")))))

;; source: https://emacsredux.com/blog/2013/06/21/eval-and-replace/
(defun eval-and-replace-sexp ()
  "Replace the preceding sexp with its value."
  (interactive)
  (right-char) ;; Fix for normal mode
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun copy-message (x)
  (kill-new x)
  (message "Copied to clipboard: %s" x))

(defun company-select-last ()
  (interactive)
  (company-select-next (- company-candidates-length 1)))

(defun math-on-number (f &optional num)
  "Read user input and apply with function f to the number at point"
  (let* ((x (thing-at-point 'number))
         (arithmetic-symbol (pcase f
                              ('+ "+")
                              ('- "-")
                              ('/ "/")
                              ('* "*")
                              (_ (error "Unknown function %s" f))))
         (readline (concat (number-to-string x) " " arithmetic-symbol " "))
         (y (or num (read-number readline)))
         (result (funcall f x y))
         (bounds (bounds-of-thing-at-point 'evil-WORD)))
    (delete-region (car bounds) (cdr bounds))
    (insert (format "%.02f" result))))

(defun math+|add-to-number ()
  (interactive)
  (math-on-number '+))

(defun math+|subtract-from-number ()
  (interactive)
  (math-on-number '-))

(defun math+|subtract-maran-vegan ()
  (interactive)
  (math-on-number '- 8.60))

(defun math+|divide-by-number ()
  (interactive)
  (math-on-number '/))

(defun math+|multiply-by-number ()
  (interactive)
  (math-on-number '*))

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

(defun find-workspace-project-root (&optional arg)
  "Gets the root dir for the current workspace"
  (--find (s-match (concat (+workspace-current-name) "/$") it) projectile-known-projects))

(defun +workspace|find-workspace-project-file ()
  (interactive)
  (cl-letf (((symbol-function 'projectile-project-root) #'find-workspace-project-root))
      (projectile-find-file)))

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
