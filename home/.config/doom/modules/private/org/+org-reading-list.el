;;; private/org/+org-reading-list.el -*- lexical-binding: t; -*-

;;; Variables

(setq-default +org-reading-list-todo-state "[ ]")

(setq-default +org-reading-list-file (concat org-directory "/reading-list.org"))

(setq-default +org-reading-list-headline "Reading List")

;;; Methods

(defun +org-reading-list/refile-to (file headline)
  "Refile an item to the reading list"
  (org-todo +org-reading-list-todo-state)
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
      (org-refile nil nil (list headline file nil pos))))

(defun +org-reading-list/refile-to-reading-list ()
  "Refile an item to the reading list"
  (interactive)
  (org-mark-ring-push)
  (+org-reading-list/refile-to +org-reading-list-file +org-reading-list-headline)
  (org-mark-ring-goto))
