;;; ~/.homesick/repos/Dotfiles/home/.config/doom/+helm.el -*- lexical-binding: t; -*-

(defun projectile-switch-project-and-do-ag (project)
  "Switch to a project and do a search"
  (let ((projectile-switch-project-action 'helm-projectile-ag))
    (projectile-switch-project-by-name project)))

(after!
  helm-projectile
  :init
  (helm-projectile-define-key helm-projectile-projects-map (kbd "C-/") 'projectile-switch-project-and-do-ag))

;; (defun ar/helm-helm (title candidates on-select-function)
;;   "Helm with TITLE CANDIDATES and ON-SELECT-FUNCTION."
;;   )

;; (defun ar/shell-send-command (command)
;;   "Send COMMAND to shell mode."
;;   ;; (assert (string-equal mode-name "Shell") nil "Not in Shell mode")
;;   (goto-char (point-max))
;;   (comint-kill-input)
;;   (insert command)
;;   (comint-send-input))

;; (defun split-up-to-semi-colon (s)
;;   "Splits a string up to the first semi-colon"
;;   (last (s-split-up-to ";" s 1)))

;; (defun opt (x y)
;;   "Helper function - When x is non-nil use x otherwise use y"
;;   (if x x y))

;; (defun parse-zsh-history (&optional file)
;;   "Read the zsh_history and parse the commands"
;;   (with-temp-buffer
;;     (insert-file-contents (opt file "~/.zsh_history") nil 0 500)
;;     (mapcar 'split-up-to-semi-colon
;;           (delete-dups
;;             (split-string (buffer-string) "\n")))))

;; (defvar +helm|zsh-history
;;   '((name . "Zsh History")
;;     (candidates-process . (lambda)))
;;   )

;; (defun +helm|zsh-history ()
;;   "Narrow down bash history with helm."
;;   (interactive)
;;   (helm :sources +helm|zsh-history
;;         :prompt  "shell command: "
;;         :buffer  "*helm shell history*"))

;; (bind-key "M-r" #'ar/helm-shell-search-history shell-mode-map)

;; ;; Save buffer name
;; ;; Close minibuffer
;; ;; Switch to bufffer
