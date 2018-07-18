;;; ~/.homesick/repos/dotfiles/home/.config/doom/+org.el -*- lexical-binding: t; -*-

(setq
 org-directory (expand-file-name "~/Dropbox/org")
 org-default-notes-file (concat org-directory "/inbox.org")
 org-shopping-list (concat org-directory "/shoppinglist.org")
 org-default-notes-list (list
                         "~/Dropbox/org/home.org"
                         "~/Dropbox/org/shoppinglist.org"
                         "~/Dropbox/org/Work/work.org"
                         "~/Dropbox/org/cooking.org"
                         "~/Dropbox/org/Projects/ideas.org"
                         )
 )

(setq org-capture-templates
      (quote (("t" "todo" entry (file+headline org-default-notes-file "INBOX")
               "* [ ] %?\n%U\n%a\n")
              ("c" "Chrome" entry (file+headline org-default-notes-file "INBOX")
               "* %?\n %(org-mac-chrome-get-frontmost-url)\n %i\n %U"
               :empty-lines 1)
              ("s" "shoppinglist" entry (file org-shopping-list)
               "* Supermarkt\n- [ ] %?")
              ("i" "idea" entry (file+headline org-default-notes-file "INBOX")
               "* %? :IDEA:\n%U\n%a\n")
              ("n" "note" entry (file+headline org-default-notes-file "INBOX")
               "* %? :NOTE:\n%U\n%a\n")
              )))

(setq org-todo-keywords
      '(
        (sequence "[ ](t)" "[-](p)" "[?](m)" "[…](w)"  "|" "[X](d)")
        ))

(setq org-todo-keyword-faces
      '(("[…]" . "grey")
        ))

(defun +org|org-open-home-file ()
   "Open the home org file"
   (interactive)
   (find-file (concat org-directory "/home.org")))

(defun +org|org-open-work-file ()
   "Open the home org file"
   (interactive)
   (find-file (concat org-directory "/Work/work.org")))

(defun +org|paste-chrome-link ()
  "Paste the frontmost chrome link"
  (interactive)
  ;; (when (not (looking-at-p "\s.*$") (
  ;;                                    (end-of-line)
  ;;                                    (new-line))))
  (insert (org-mac-chrome-get-frontmost-url))
  )

(defun +org|paste-markdown-as-org ()
  "Convert the current clipboard to markdown"
  (interactive)
  (insert (shell-command-to-string "pbpaste | pandoc -f markdown -t org"))
  )

(map! :leader (
               :desc "Notes" :prefix "n"
                     :desc "Home" :n  "h" #'+org|org-open-home-file
                     :desc "Inbox" :n  "i" (λ! (find-file (concat org-directory "/inbox.org")))
                     :desc "Work" :n  "w" #'+org|org-open-work-file
                     :desc "Agenda" :n  "a" #'org-agenda
                     :desc "Store Link" :n  "y" #'org-store-link
                     :desc "Save All Org Buffers" :n  "S" #'org-save-all-org-buffers
        ))

;; Journal

(defvar org-journal-dir-default "~/Dropbox/org/journal")
(defvar org-journal-dir-diary "~/Dropbox/org/diary")
(setq org-journal-dir org-journal-dir-default)

(setq org-journal-file-format "%Y-%m-%d")
(setq org-journal-date-prefix "#+TITLE: ")
(setq org-journal-date-format "%A, %B %d %Y")
(setq org-journal-time-prefix "* ")
(setq org-journal-time-format "")

(after! org
  (map! :map evil-org-mode-map

        :n "M-k" #'org-move-subtree-up
        :n "M-j" #'org-move-subtree-down

        :localleader
        :desc "Archive Subtree" :m "a" #'org-archive-subtree
        :desc "Paste Chrome Link" :m "p" #'+org|paste-chrome-link

        :desc "Create/Edit Todo" :nve "o" #'org-todo
        :desc "Schedule" :nve "s" #'org-schedule
        :desc "Deadline" :nve "d" #'org-deadline
        :desc "Refile" :nve "r" #'org-refile
        :desc "Filter" :nve "f" #'org-match-sparse-tree
        :desc "Tag heading" :nve "t" #'org-set-tags-command)

  :config

  ;; Templates
  (add-to-list 'org-structure-template-alist '("e" "#+BEGIN_SRC elisp\n?\n#+END_SRC\n"))
  (add-to-list 'org-structure-template-alist '("j" "#+BEGIN_SRC js\n?\n#+END_SRC\n"))
  (add-to-list 'org-structure-template-alist '("b" "#+BEGIN_SRC bash\n?\n#+END_SRC\n"))
  (setq
   org-image-actual-width 600
   org-agenda-files (append (list "~/Dropbox/org") (list "~/Dropbox/org/Work"))
   org-agenda-refile (org-agenda-files)
   org-refile-targets (quote (
                              (nil :maxlevel . 5)
                              (org-agenda-files :maxlevel . 3)
                              ))
   org-default-notes-file (concat org-directory "/inbox.org")
   )
  )
