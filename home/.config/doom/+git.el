;;; ~/.homesick/repos/Dotfiles/home/.config/doom/+git.el -*- lexical-binding: t; -*-

(setq-default magit-save-repository-buffers 'dontask)

(defun shell-command-to-list (cmd)
  "Split output from shell-command to list"
  (split-string (shell-command-to-string cmd) "\n" t))

(defun git-new-files ()
  (shell-command-to-list "git ls-files -om --exclude-standard"))

(defun git-modified-files (branch)
  (shell-command-to-list
   (format "git --no-pager diff --no-renames --name-only --no-merges %s master;" (magit-rev-parse "HEAD"))))

(defun git-get-changed-files (b)
    (delete-dups (append (git-modified-files b) (git-new-files))))

(defun +git|helm-changed-files ()
  (interactive)
  (helm :sources (helm-build-sync-source "Git Changed Files"
                   :candidates (git-get-changed-files "master")
                   :action (helm-make-actions
                            "Find file" (lambda (fs) (find-file (concat (projectile-project-root) fs)))
                            "Search" (lambda (fs) (helm-do-ag nil fs)))
                   :fuzzy-match t)
        :buffer "*helm git changed files"))

(defun magit-revision-show-original-file ()
  "Show the orginal file from a revision buffer
If possible also go to the pointing line"
  (interactive)
  (when magit-buffer-file-name
    (let ((file-name magit-buffer-file-name)
          (line-number (line-number-at-pos))
          (current-line (thing-at-point 'line t)))
      (delete-other-windows)
      (find-file file-name))))
      ;; (when (string= (thing-at-point 'line t) 'current-line)
      ;;   (message "SAME LINE")
      ;;   (goto-line line-number))


(defun +git|commit-search-message-history ()
  "Search and insert commit message from history."
  (interactive)
  (insert (completing-read "History: "
                           ;; Remove unnecessary newlines from beginning and end.
                           (mapcar (lambda (text)
                                     (string-trim text))
                                   (ring-elements log-edit-comment-ring)))))
(defun +git|undo ()
  "Soft reset current git repo to HEAD~1."
  (interactive)
  (magit-reset-soft "HEAD~1"))

(after! magit
  :config
  (setq
   magithub-clone-default-directory "~/Code/Repositories"
   git-commit-summary-max-length 120))
  ;; (bind-key "M-r" #'+git|commit-search-message-history git-commit-mode-map)
  ;; (add-to-list 'savehist-additional-variables log-edit-comment-ring))

(map!
 :leader
 (:desc "Magit" :prefix "g"
   :desc "Changed Files" :n  "F" #'+git|helm-changed-files
   :desc "Fetch" :n  "f" #'magit-fetch-popup
   :desc "Undo" :n  "u" #'+git|undo))
