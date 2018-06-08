(defconst floscr-org-packages
  '(
     org
     spacemacs-org
     spacemacs-journal
     ))

(defun floscr-org/insert-key (key)
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
  ;; Org directories
  (setq floscr/home-dir (expand-file-name "~"))
  (setq org-directory (concat floscr/home-dir "/Dropbox/org"))

  (setq org-default-notes-file (concat org-directory "/inbox.org"))


  ;; Allow blank lines before and after headlines
  (custom-set-variables
   '(org-blank-before-new-entry 
     (quote ((heading) (plain-list-item))))

  ;; capture
  (setq org-capture-templates
        (quote (("t" "todo" entry (file org-default-notes-file)
                 "* TODO %?\n%U\n%a\n")
                ("m" "meeting" entry (file org-default-notes-file)
                 "* MEETING with %? :MEETING:\n%U")
                ("i" "idea" entry (file org-default-notes-file)
                 "* %? :IDEA:\n%U\n%a\n")
                ("n" "note" entry (file org-default-notes-file)
                 "* %? :NOTE:\n%U\n%a\n")
                ("h" "habit" entry (file rae/org-default-notes-file)
                 "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

  ;; refile
  (setq org-refile-targets '(
                              (nil :maxlevel . 3)
                              (org-agenda-files :maxlevel . 3)
                              ))

  (setq org-agenda-files (list "~/Dropbox/org/home.org"
                            "~/Dropbox/org/projects.org"
                            "~/.org-annotate-file.org"
                            "~/Dropbox/org/refile-beorg.org"
                            "~/Dropbox/org/Einkaufsliste.org"
                            "~/Dropbox/org/Work/Work.org"
                            ))
   (setq org-journal-dir "~/Dropbox/org/journal")
   (setq org-journal-file-format "%Y-%m-%d")
   (setq org-journal-date-prefix "#+TITLE: ")
   (setq org-journal-date-format "%A, %B %d %Y")
   (setq org-journal-time-prefix "* ")
   (setq org-journal-time-format "")

  )
