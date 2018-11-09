;;; ~/.homesick/repos/Dotfiles/home/.config/doom/+utils.el -*- lexical-binding: t; -*-

(defun copy-message (x)
  (kill-new x)
  (message "Copied to clipboard: %s" x))

(defun company-select-last ()
  (interactive)
  (company-select-next (- company-candidates-length 1)))

;; From https://gist.github.com/cbowdon/012d623920bd28453bf8
(defmacro template (text)
  "Expand text like \"Hello <<name>>\" to (format \"Hello %s\" name)."
  (let ((pattern "<<\\(.*?\\)>>"))
    ;; The regexp matches anything between delimiters, non-greedily
    (with-temp-buffer
      (save-excursion (insert text))
      (let ((matches '()))
        (while (re-search-forward pattern nil t)
          (push (match-string 1) matches)
          (replace-match "%s" t t))
        `(format ,(buffer-string) ,@(reverse (mapcar 'read matches)))))))

(defun math-on-number (f)
  "Read user input and apply with function f to the number at point"
  (let* ((x (thing-at-point 'number))
         (arithmetic-symbol (pcase f
                              ('+ "+")
                              ('- "-")
                              ('/ "/")
                              ('* "*")
                              (_ (error "Unknown function %s" f))))
         (readline (concat (number-to-string x) " " arithmetic-symbol " "))
         (y (read-number readline))
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

(defun find-workspace-project-root ()
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
