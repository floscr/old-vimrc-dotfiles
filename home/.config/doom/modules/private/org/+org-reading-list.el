;;; private/org/+org-reading-list.el -*- lexical-binding: t; -*-

;;; Variables

(setq-default +org-reading-list-todo-state "[ ]")
(setq-default +org-reading-list-file (concat org-directory "/reading-list.org"))
(setq-default +org-reading-list-headline "Reading List")
(setq-default +org-watching-list-headline "Watching List")
(setq-default +org-listening-list-headline "Listening List")

;;; Utils

(defun +org-reading-list/save (file headline &optional todo-state set-tags-p)
  "Refile the item under the cursor to a FILE HEADLINE with a todo-state"
  (org-mark-ring-push)
  (when todo-state (org-todo todo-state))
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
    (org-refile nil nil (list headline file nil pos)))
  (when set-tags-p (org-set-tags-command))
  (org-mark-ring-goto))

;;; Methods

(defun +org-reading-list/refile-to-reading-list ()
  "Refile an item to the reading list"
  (interactive)
  (+org-reading-list/save
   +org-reading-list-file
   +org-reading-list-headline
   +org-reading-list-todo-state
   t))

(defun +org-reading-list/refile-to-watching-list ()
  "Refile an item to the reading list"
  (interactive)
  (+org-reading-list/save
   +org-reading-list-file
   +org-watching-list-headline
   +org-reading-list-todo-state
   t))

(defun +org-reading-list/refile-to-listening-list ()
  "Refile an item to the reading list"
  (interactive)
  (+org-reading-list/save
   +org-reading-list-file
   +org-listening-list-headline
   +org-reading-list-todo-state
   t))
