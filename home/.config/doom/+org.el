;;; ~/.homesick/repos/dotfiles/home/.config/doom/+org.el -*- lexical-binding: t; -*-

(setq projectile-globally-ignored-file-suffixes '(".org_archive"))

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

(setq org-agenda-start-day "-1d")
(setq org-agenda-span 5)

(evil-define-key 'motion org-agenda-mode-map
  "vd" 'org-agenda-day-view
  "ds" 'org-agenda-schedule
  "vw" 'org-agenda-week-view
  "vm" 'org-agenda-month-view
  "vy" 'org-agenda-year-view
)

(setq org-capture-templates
      (quote (("t" "todo" entry (file+headline org-default-notes-file "INBOX")
               "* [ ] %?\n%U")
              ("c" "Chrome" entry (file+headline org-default-notes-file "INBOX")
               "* %(org-mac-chrome-get-frontmost-url)%?\n%U"
               )
              ("s" "shoppinglist" entry (file org-shopping-list)
               "* Supermarkt\n- [ ] %?")
              ("i" "idea" entry (file+headline org-default-notes-file "INBOX")
               "* %? :IDEA:\n%U")
              ("f" "file" entry (file+headline org-default-notes-file "INBOX")
               "* %?\n%U\n%a")
              ("b" "book" entry (file+headline "~/Dropbox/org/books.org" "Read in the future")
               "*** %?\n%U")
              ("n" "note" entry (file+headline org-default-notes-file "INBOX")
               "* %? :NOTE:\n%U")
              )))

(setq org-todo-keywords
      '(
        (sequence "[ ](t)" "[-](p)" "[?](m)" "[…](w)"  "|" "[X](d)")
        ))

(setq org-todo-keyword-faces
      '(("[…]" . "grey")
        ))

(defun +org|find-file (f)
  "Find file in org directory"
  (find-file (concat org-directory f)))

(defun +org|org-open-home-file ()
   "Open the home org file"
   (interactive)
    (+org|find-file "/home.org"))

(defun +org|org-open-reading-list-file ()
   "Open the reading list org file"
   (interactive)
    (+org|find-file "/Collections/reading-list.org"))

(defun +org|org-open-work-file ()
   "Open the home org file"
   (interactive)
    (+org|find-file "/Work/work.org"))

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

(defun +org|copy-block ()
  "Copies the current block to clipboard"
  (interactive)
  (org-edit-src-code)
  (clipboard-kill-ring-save
   (point-min)
   (point-max))
  (org-edit-src-abort))

(defun +org|sort-entries ()
  "Go to header and sort entries"
  (interactive)
  (org-up-element)
  (org-sort)
  (org-shifttab)
  (org-cycle))

(defun +org|narrow-to-subtree ()
  "Narrow to subtree and disable org-indent-mode"
  (interactive)
  (org-narrow-to-subtree)
  (org-indent-mode -1))

(defun +org|narrow-to-block ()
  "Narrow to subtree and disable org-indent-mode"
  (interactive)
  (org-narrow-to-block)
  (org-indent-mode -1))

(defun +org|narrow-to-element ()
  "Narrow to subtree and disable org-indent-mode"
  (interactive)
  (org-narrow-to-element)
  (org-indent-mode -1))

(defun +org|widen ()
  "Widen and enable org-indent-mode"
  (interactive)
  (widen)
  (org-indent-mode t)
  (recenter nil))

(defun +org|paste-chrome-link ()
  "Paste the frontmost chrome link
Fixes wrong paste behaviour where the link would be inserted directly on the character by adding a space
E.g.: (Brackets signal the cursor position)
**[*]
***[]"
  (interactive)
  (when (not (looking-at-p "[\s\t\n\r]"))
    (forward-char))
  (insert-char " ")
  (insert (org-mac-chrome-get-frontmost-url)))

(defun +org|grab-tabs ()
  "Grab all the chrome tabs as an org list to save for later inspection"
  (interactive)
  (let ((tabs
         (do-jxa-script
          (concat
           "Application(\"Chrome\").windows[0].tabs()"
           ".map(tab => `"
           "- [[${tab.url()}][${tab.title()}]]"
           "`)"
           ".join(\"\\n\")"))))
    (insert tabs)))

(map! :leader (
               :desc "Notes" :prefix "n"
                     :desc "Home" :n  "h" #'+org|org-open-home-file
                     :desc "Reading List" :n  "r" #'+org|org-open-reading-list-file
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

;; (add-to-list '+org-capture-frame-parameters '(left . 0.5))
;; (add-to-list '+org-capture-frame-parameters '(width . 110))

(after! org
  (map! :map evil-org-mode-map

        :n "M-k" #'org-move-subtree-up
        :n "M-j" #'org-move-subtree-down

        :localleader
        :desc "Archive Subtree"   :m "a" #'org-archive-subtree
        :desc "Paste Chrome Link" :m "p" #'+org|paste-chrome-link
        :desc "Grab tabs"         :m "P" #'+org|grab-tabs
        :desc "Cut Subtree"       :m "C" #'org-cut-subtree
        :desc "Paste Subtree"     :m "P" #'org-paste-subtree
        :desc "Sort Entries"      :m "S" #'+org|sort-entries
        (
         :desc "Insert" :prefix "i"
               :desc "Subheadeing" :m "s" (λ!
                                           (call-interactively 'org-insert-subheading)
                                           (evil-insert-state))
         )

        (
         :desc "Narrow" :prefix "n"
               :desc "Subtree" :m "s" #'+org|narrow-to-subtree
               :desc "Block"   :m "b" #'+org|narrow-to-block
               :desc "Element" :m "e" #'+org|narrow-to-element
               :desc "widen"   :m "w" #'+org|widen
         )

        :desc "Create/Edit Todo"  :nve "o" #'org-todo
        :desc "Schedule"          :nve "s" #'org-schedule
        :desc "Deadline"          :nve "d" #'org-deadline
        :desc "Refile"            :nve "r" #'org-refile
        :desc "Filter"            :nve "f" #'org-match-sparse-tree
        :desc "Tag heading"       :nve "t" #'org-set-tags-command)

  :config

  ;; Templates
  (add-to-list 'org-structure-template-alist '("e" "#+BEGIN_SRC elisp\n?\n#+END_SRC\n"))
  (add-to-list 'org-structure-template-alist '("E" "#+BEGIN_EXAMPLE\n?\n#+END_EXAMPLE"))
  (add-to-list 'org-structure-template-alist '("j" "#+BEGIN_SRC js\n?\n#+END_SRC\n"))
  (add-to-list 'org-structure-template-alist '("ps" "#+BEGIN_SRC purescript\n?\n#+END_SRC\n"))
  (add-to-list 'org-structure-template-alist '("b" "#+BEGIN_SRC bash\n?\n#+END_SRC\n"))

  (defun level-1-refile-targets ()
    (list "~/Dropbox/org/Collections/reading-list.org"))

  (defun level-2-refile-targets ()
    (list "~/Dropbox/org/Collections/Emacs.org"))

  (setq
   org-agenda-start-on-weekday 1
   org-image-actual-width 600
   org-agenda-files (append (list "~/Dropbox/org") (list "~/Dropbox/org/Work"))
   org-refile-targets (quote (
                              (nil :maxlevel . 5)
                              (org-agenda-files :maxlevel . 2)
                              (level-2-refile-targets :level . 2)
                              (level-1-refile-targets :level . 1)
                              ))
   org-agenda-refile org-agenda-files
   org-default-notes-file (concat org-directory "/inbox.org")
   )
  )
