(defconst floscr-org-packages
  '(
     org
     spacemacs-org
     spacemacs-journal
     ))

(defvar org-journal-dir-default "~/Dropbox/org/journal")
(defvar org-journal-dir-diary "~/Dropbox/org/diary")

(defun floscr-org/activate-journal ()
  (interactive)
  (setq org-journal-dir org-journal-dir-default)
  (setq org-journal-enable-encryption nil)
  )

(defun floscr-org/activate-diary ()
  (setq org-journal-dir org-journal-dir-diary)
  (setq org-journal-enable-encryption 1)
  )

(defun floscr-org/switch-diary ()
  (interactive)
  (if (eq org-journal-dir org-journal-dir-default)
      (floscr-org/activate-diary)
    (floscr-org/activate-journal))
  (message "Switch main diary to %s" org-journal-dir)
  )

(defun floscr-org/shopping-list-save-hook ()
  "Save the contents of supermarkt to a text file to make it readable for notes app"
  (interactive)
  ;; Keep the cursor position
  (save-excursion
    (require 's)
    (require 'f)

    ;; Copy subtree of Supermarkt
    (goto-char (org-find-exact-headline-in-buffer "Supermarkt"))
    (setq org-subtree-clip-backup org-subtree-clip)
    (org-copy-subtree)

    (setq subtree-contents (s-replace "*" "#" org-subtree-clip))
    (f-write-text subtree-contents 'utf-8 "~/Dropbox/Notes/Shoppinglist.txt")

    ;; Restore previous subtree-clip
    (setq org-subtree-clip org-subtree-clip-backup)
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

  (setq org-image-actual-width 600)

  (add-to-list 'org-structure-template-alist '("e" "#+BEGIN_SRC elisp\n?\n#+END_SRC\n"))
  (add-to-list 'org-structure-template-alist '("j" "#+BEGIN_SRC js\n?\n#+END_SRC\n"))
  (add-to-list 'org-structure-template-alist '("b" "#+BEGIN_SRC bash\n?\n#+END_SRC\n"))

  (custom-set-faces
   '(org-level-2 ((t (:inherit outline-2 :height 1.0 ))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.0 ))))
   '(org-level-4 ((t (:inherit outline-3 :height 1.0 ))))
   '(org-level-5 ((t (:inherit outline-3 :height 1.0 ))))
   '(org-level-6 ((t (:inherit outline-3 :height 1.0 ))))
   '(org-level-7 ((t (:inherit outline-3 :height 1.0 ))))
   '(org-level-8 ((t (:inherit outline-3 :height 1.0 ))))
   )

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
                ("c" "Chrome" entry (file+headline "~/.emacs.d/gtd.org" "Quick notes")
                 "* TODO [#C] %?\n %(org-mac-chrome-get-frontmost-url)\n %i\n %U"
                 :empty-lines 1)
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
         "~/.org-annotate-file.org"
         "~/Dropbox/org/refile-beorg.org"
         "~/Dropbox/org/shoppinglist.org"
         "~/Dropbox/org/Work/Work.org"
         "~/Dropbox/org/cooking.org"
         "~/Dropbox/org/Projects/ideas.org"
         ))

  (setq org-agenda-refile (org-agenda-files))

  (setq
   org-refile-targets
   '(
     (org-agenda-files :maxlevel . 3)
     ))

   (setq org-journal-dir org-journal-dir-default)
   (setq org-journal-file-format "%Y-%m-%d")
   (setq org-journal-date-prefix "#+TITLE: ")
   (setq org-journal-date-format "%A, %B %d %Y")
   (setq org-journal-time-prefix "* ")
   (setq org-journal-time-format "")

  )
