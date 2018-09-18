;;; ~/.homesick/repos/Dotfiles/home/.config/doom/+helm.el -*- lexical-binding: t; -*-

(defun projectile-switch-project-and-do-ag (project)
  "Switch to a project and do a search"
  (let ((projectile-switch-project-action 'helm-projectile-ag))
    (projectile-switch-project-by-name project)))

(after!
  helm-projectile
  :config
  (helm-projectile-define-key helm-projectile-projects-map (kbd "C-/") 'projectile-switch-project-and-do-ag))

(defun ar/helm-helm (title candidates on-select-function)
  "Helm with TITLE CANDIDATES and ON-SELECT-FUNCTION."
  (helm :sources `((name . ,title)
                   (candidates . ,candidates)
                   (action . ,on-select-function))
        :buffer "*helm-exec*"
        :candidate-number-limit 10000))

(defun ar/shell-send-command (command)
  "Send COMMAND to shell mode."
  ;; (assert (string-equal mode-name "Shell") nil "Not in Shell mode")
  (goto-char (point-max))
  (comint-kill-input)
  (insert command)
  (comint-send-input))

(defun find-zsh-command-from-history-string (s)
  (last (s-split-up-to ";" s 1)))

(defun ar/helm-shell-search-history ()
  "Narrow down bash history with helm."
  (interactive)
  ;; (assert (string-equal mode-name "Shell") nil "Not in Shell mode")
  (ar/helm-helm "bash history"
                (with-temp-buffer
                  (insert-file-contents "~/.zsh_history")

                  (mapc 'find-zsh-command-from-history-string
                        (reverse
                         (delete-dups
                          (split-string (buffer-string) "\n")))))
                #'ar/shell-send-command))

(bind-key "M-r" #'ar/helm-shell-search-history shell-mode-map)
