;;; ~/.homesick/repos/dotfiles/home/.config/doom/+org.el -*- lexical-binding: t; -*-

(setq projectile-globally-ignored-file-suffixes '(".org_archive"))

(setq
 org-directory (expand-file-name "~/Dropbox/org")
 org-default-notes-file (concat org-directory "/inbox.org")
 org-shopping-list (concat org-directory "/shoppinglist.org")

 org-agenda-start-day "-1d"
 org-agenda-span 5)

(evil-define-key 'motion org-agenda-mode-map
  "vd" 'org-agenda-day-view
  "ds" 'org-agenda-schedule
  "vw" 'org-agenda-week-view
  "vm" 'org-agenda-month-view
  "vy" 'org-agenda-year-view)

(setq org-capture-templates
      (quote (("t" "todo" entry (file+headline org-default-notes-file "INBOX")
               "* [ ] %?\n%U")
              ("c" "Chrome" entry (file+headline org-default-notes-file "INBOX")
               "* %(org-mac-chrome-get-frontmost-url)%?\n%U")

              ("s" "shoppinglist" entry (file org-shopping-list)
               "* Supermarkt\n- [ ] %?")
              ("i" "idea" entry (file+headline org-default-notes-file "INBOX")
               "* %? :IDEA:\n%U")
              ("f" "file" entry (file+headline org-default-notes-file "INBOX")
               "* %?\n%U\n%a")
              ("b" "book" entry (file+headline "~/Dropbox/org/books.org" "Read in the future")
               "*** %?\n%U")
              ("n" "note" entry (file+headline org-default-notes-file "INBOX")
               "* %? :NOTE:\n%U"))))



(defun org-find-file (f)
  "Find file in org directory"
  (find-file (concat org-directory f)))

(defun my-archive-entry ()
  (message "%s" (thing-at-point 'line t)))

(defun +org|org-archive-done-tasks ()
  (interactive)
  (org-map-entries #'my-archive-entry "/[X]" 'tree))

(defun +org|org-archive-done-task ()
  (interactive)
  (org-map-entries (lambda (file) (message file)) "/[X]" 'file))

(defun +org|org-open-home-file ()
  "Open the home org file"
  (interactive)
  (org-find-file "/home.org"))

(defun +org|org-open-reading-list-file ()
  "Open the reading list org file"
  (interactive)
  (org-find-file "/reading-list.org"))

(defun +org|org-open-work-file ()
  "Open the home org file"
  (interactive)
  (org-find-file "/Work/work.org"))

(defun +org|paste-markdown-as-org ()
  "Convert the current clipboard to markdown"
  (interactive)
  (insert (shell-command-to-string "pbpaste | pandoc -f markdown -t org")))

(defun +org|copy-block ()
  "Copies the current block to clipboard"
  (interactive)
  (org-edit-src-code)
  (clipboard-kill-ring-save (point-min) (point-max))
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
  (unless (looking-at-p "[\s\t\n\r]") (forward-char))
  (insert " ")
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

(defun +org|org-src-block-refmt-reason-ocaml-toggle ()
  "Convert the current src block from ocaml to reason and vice versa"
  (interactive)
  (save-excursion
    (let* ((old-block (org-element-at-point))
           (old-lang (org-element-property :language old-block))
           (new-lang (if (string= old-lang "ocaml") "reason" "ocaml"))
           (formatter (if (string= old-lang "ocaml") 'refmt-region-ocaml-to-reason 'refmt-region-reason-to-ocaml)))
      (org-edit-special)
      (funcall formatter (point-min) (point-max))
      (org-edit-src-exit)
      (let* ((new-block (org-element-at-point))
             (new-block-parsed (org-element-interpret-data (org-element-put-property (org-element-at-point) :language new-lang)))
             (from (org-element-property :begin new-block))
             (to (org-element-property :end new-block)))
        (delete-region from to)
        (insert new-block-parsed)))))

(map! :leader (
               :desc "Notes" :prefix "n"
               :desc "Home" :n  "h" #'+org|org-open-home-file
               :desc "Reading List" :n  "r" #'+org|org-open-reading-list-file
               :desc "Inbox" :n  "i" (λ! (find-file (concat org-directory "/inbox.org")))
               :desc "Work" :n  "w" #'+org|org-open-work-file
               :desc "Agenda" :n  "a" #'org-agenda
               :desc "Store Link" :n  "y" #'org-store-link
               :desc "Save All Org Buffers" :n  "S" #'org-save-all-org-buffers))


;; Journal

(defvar org-journal-dir-default "~/Dropbox/org/journal")
(defvar org-journal-dir-diary "~/Dropbox/org/diary")
(setq org-journal-dir org-journal-dir-default)

(setq org-journal-file-format "%Y-%m-%d")
(setq org-journal-date-prefix "#+TITLE: ")
(setq org-journal-date-format "%A, %B %d %Y")
(setq org-journal-time-prefix "* ")
(setq org-journal-time-format "")


(after! org-agenda
  ;; (setq org-agenda-custom-commands '())
  (add-to-list 'org-agenda-custom-commands
               '("p" "Private" agenda ""
                 ((org-agenda-ndays 5)
                  (org-agenda-span 7)
                  (org-agenda-tag-filter-preset '("-WORK" "-REPEATING"))
                  (tags-todo "-\[X\]")
                  (tags-todo "-DONE")
                  (org-agenda-start-on-weekday 0)
                  (org-agenda-time-grid nil)
                  (org-agenda-start-on-weekday 1)
                  (org-agenda-repeating-timestamp-show-all t))))
  (add-to-list 'org-agenda-custom-commands
               '("w" "Work" tags-todo "+WORK"))
  (add-to-list 'org-agenda-custom-commands
               '("rr" "Reading List" tags-todo "+TEXT")))

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

        :desc "Create/Edit Todo"  :nve "o" #'org-todo
        :desc "Schedule"          :nve "s" #'org-schedule
        :desc "Deadline"          :nve "d" #'org-deadline
        :desc "Refile"            :nve "r" #'org-refile
        :desc "Filter"            :nve "f" #'org-match-sparse-tree
        :desc "Tag heading"       :nve "t" #'org-set-tags-command

        (:desc "Insert" :prefix "i"
          :desc "Subheadeing" :m "s" (λ!
                                      (call-interactively 'org-insert-subheading)
                                      (evil-insert-state))
          :desc "Inavtive Timestamp" :m "i" 'org-time-stamp-inactive)
        (:desc "Narrow" :prefix "n"
          :desc "Subtree" :m "s" #'+org|narrow-to-subtree
          :desc "Block"   :m "b" #'+org|narrow-to-block
          :desc "Element" :m "e" #'+org|narrow-to-element
          :desc "widen"   :m "w" #'+org|widen))

  :config

  (setq
   org-todo-keywords '((sequence "[ ](t)" "|" "[X](d)")
                       (sequence "TODO(T)" "DOING(D)" "NEXT(N)" "LATER(L)" "QUESTION(Q)" "|" "DONE(X)" "CANCELLED(C)" "WAITING(W)")))

  ;; Templates
  ;; TODO: Solve this with https://github.com/plexus/a.el
  (add-to-list 'org-structure-template-alist '("e" "#+BEGIN_SRC elisp\n?\n#+END_SRC\n"))
  (add-to-list 'org-structure-template-alist '("E" "#+BEGIN_EXAMPLE\n?\n#+END_EXAMPLE"))
  (add-to-list 'org-structure-template-alist '("j" "#+BEGIN_SRC js\n?\n#+END_SRC\n"))
  (add-to-list 'org-structure-template-alist '("ps" "#+BEGIN_SRC purescript\n?\n#+END_SRC\n"))
  (add-to-list 'org-structure-template-alist '("b" "#+BEGIN_SRC bash\n?\n#+END_SRC\n"))
  (add-to-list 'org-structure-template-alist '("re" "#+BEGIN_SRC reason\n?\n#+END_SRC\n"))
  (add-to-list 'org-structure-template-alist '("oc" "#+BEGIN_SRC ocaml\n?\n#+END_SRC\n"))
  (add-to-list 'org-structure-template-alist '("rb" "#+BEGIN_SRC ruby\n?\n#+END_SRC\n"))
  (add-to-list 'org-structure-template-alist '("md" "#+BEGIN_SRC markdown\n?\n#+END_SRC\n"))

  (defun expand-org-file-names (xs)
    (mapcar (λ (x) (expand-file-name x org-directory)) xs))

  (setq level-1-refile-targets (expand-org-file-names '("reading-list.org"
                                                        "cooking.org"
                                                        ;; "books.org"
                                                        "programming.org"
                                                        "shoppinglist.org")))

  (setq max-level-2-refile-targets (expand-org-file-names '("Emacs.org"
                                                            "art.org"
                                                            "diary"
                                                            "games.org"
                                                            "hardware.org"
                                                            "home.org"
                                                            "inbox.org"
                                                            "mealplan.org"
                                                            "misc.org"
                                                            "movies.org"
                                                            "music.org"
                                                            "osx.org"
                                                            "personal.org"
                                                            "podcasts.org"
                                                            "projects.org"
                                                            "sleep.org"
                                                            "sports.org"
                                                            "travel.org"
                                                            "Work/work.org")))

  (setq org-agenda-files (-concat level-1-refile-targets max-level-2-refile-targets))

  (defun level-1-refile-targets () level-1-refile-targets)

  (defun max-level-2-refile-targets () max-level-2-refile-targets)

  (setq
   org-agenda-start-on-weekday 1
   org-image-actual-width 600
   org-refile-targets (quote ((nil :maxlevel . 5)
                              (max-level-2-refile-targets :maxlevel . 2)
                              (level-1-refile-targets :level . 1)))
   org-agenda-refile org-agenda-files
   org-default-notes-file (concat org-directory "/inbox.org")))
