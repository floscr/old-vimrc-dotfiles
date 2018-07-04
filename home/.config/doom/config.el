;;; ~/.homesick/repos/Dotfiles/home/.config/doom/config.el -*- lexical-binding: t; -*-

(setq org-directory (expand-file-name "~/Dropbox/org"))
(setq org-default-notes-file (concat org-directory "/inbox.org"))
(setq org-shopping-list (concat org-directory "/shoppinglist.org"))

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

(setq org-todo-keywords
      '((sequence "TODO(t)" "SUBTREE(s)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELLED(c@/!)")
        (sequence "CRASH(c)" "BUG(b)" "REQUEST(r)" "TEST(e)" "|" "FIXED(f)")))

(setq org-todo-keyword-faces
      '(("WAIT" . "white")
        ("CRASH" . "red")
        ("BUG" . "red")
        ("SUBTREE" . "grey")
        ("TEST" . "turquoise1")
        ))

(defun +org|org-open-home-file ()
   "Open the home org file"
   (interactive)
   (find-file (concat org-directory "/home.org")))

(map! :leader
      (:desc "open" :prefix "n"
        :desc "Org Agenda" :n  "a" #'org-agenda
        :desc "Org Home" :n  "h" #'+org|org-open-home-file
        ))

(after! org
  :config
  (setq org-image-actual-width 600)

  (add-to-list 'org-structure-template-alist '("e" "#+BEGIN_SRC elisp\n?\n#+END_SRC\n"))
  (add-to-list 'org-structure-template-alist '("j" "#+BEGIN_SRC js\n?\n#+END_SRC\n"))
  (add-to-list 'org-structure-template-alist '("b" "#+BEGIN_SRC bash\n?\n#+END_SRC\n"))


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

  (setq org-agenda-refile (org-agenda-files))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  )
