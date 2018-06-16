(defconst floscr-org-packages
  '(
     org
     spacemacs-org
     spacemacs-journal
     ))

(defun floscr-org/insert-key-binding-tag (key)
  "Interactive enter a keybinding and automatically insert it into <kbd> tags"
  (interactive "kType key sequence: ")
  (let* ((is-org-mode (derived-mode-p 'org-mode))
         (tag (if is-org-mode
                  "@@html:<kbd>%s</kbd>@@"
                "<kbd>%s</kbd>")))
    (if (null (equal key "\r"))
        (insert
         (format tag (help-key-description key nil)))
      (insert (format tag ""))
      (forward-char (if is-org-mode -8 -6)))))

(defun floscr-org/post-init-org ()
  (require 'org-projectile)

  ;; Allow blank lines before and after headlines
  (custom-set-variables
   '(org-blank-before-new-entry
     (quote ((heading) (plain-list-item)))))

  ;; Org directories
  (setq org-directory (expand-file-name "~/Dropbox/org"))

  (setq org-default-notes-file (concat org-directory "/inbox.org"))
  (setq org-shopping-list (concat org-directory "/shoppinglist.org"))

  (setq org-projectile-per-project-filepath "Tasks/tasks.org")
  (push (org-projectile-project-todo-entry) org-capture-templates)
  ;; (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))

  ;; capture
  (setq org-capture-templates
        (quote (("t" "todo" entry (file org-default-notes-file)
                 "* TODO %?\n\t%U\n\t%a\n")
                ("s" "shoppinglist" entry (file org-shopping-list)
                 "* Supermarkt\n\t- [ ] %?")
                ("i" "idea" entry (file org-default-notes-file)
                 "* %? :IDEA:\n\t%U\n\t%a\n")
                ("n" "note" entry (file org-default-notes-file)
                 "* %? :NOTE:\n\t%U\n\t%a\n")
                )))

  (setq org-agenda-files
        (list
         "~/Dropbox/org/home.org"
         "~/Dropbox/org/inbox.org"
         "~/Dropbox/org/refile-beorg.org"
         "~/Dropbox/org/shoppinglist.org"
         "~/Dropbox/org/Work/Work.org"
         "~/Dropbox/org/cooking.org"
         "~/Dropbox/org/Projects/ideas.org"
         ))

  (setq
   org-refile-targets
   '(
     (org-directory :maxlevel . 3)
     (org-agenda-files :maxlevel . 3)
     ))

   (setq org-journal-dir "~/Dropbox/org/journal")
   (setq org-journal-file-format "%Y-%m-%d")
   (setq org-journal-date-prefix "#+TITLE: ")
   (setq org-journal-date-format "%A, %B %d %Y")
   (setq org-journal-time-prefix "* ")
   (setq org-journal-time-format "")

  )
