;;; ~/.homesick/repos/Dotfiles/home/.config/doom/+git.el -*- lexical-binding: t; -*-

(setq-default magit-save-repository-buffers 'dontask)

(defun shell-command-to-list (cmd)
  "Split output from shell-command to list"
  (split-string (shell-command-to-string cmd) "\n" t))

(defun git-new-files ()
  (shell-command-to-list "git ls-files -om --exclude-standard"))

(defun git-modified-files (branch)
  (shell-command-to-list
   (format "git --no-pager diff --no-renames --name-only --no-merges %s master;" (magit-get-current-branch))))

(defun git-get-changed-files (b)
    (delete-dups (append (git-modified-files b) (git-new-files))))

(defun +git|helm-changed-files ()
    (interactive)
  (helm :sources (helm-build-sync-source "Git Changed Files"
                   :candidates (git-get-changed-files "master")
                   :action (lambda (f) (find-file (concat (projectile-project-root) f)))
                   :fuzzy-match t)
        :buffer "*helm git changed files"))

(defun +git|commit-search-message-history ()
  "Search and insert commit message from history."
  (interactive)
  (insert (completing-read "History: "
                           ;; Remove unnecessary newlines from beginning and end.
                           (mapcar (lambda (text)
                                     (string-trim text))
                                   (ring-elements log-edit-comment-ring)))))

(after! magit
  :config
  (bind-key "M-r" #'+git|commit-search-message-history git-commit-mode-map)
  (add-to-list 'savehist-additional-variables log-edit-comment-ring)
  (savehist-mode +1)
  )

(map!
 :leader
 (:desc "Magit" :prefix "g"
   :desc "Changed Files" :n  "F" #'+git|helm-changed-files
   :desc "Fetch" :n  "f" #'magit-fetch-popup))
