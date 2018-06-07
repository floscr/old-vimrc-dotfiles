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

  (setq org-default-notes-file (concat org-directory "inbox.org"))

  (setq
   ;; Add files to refile targets
   ;; , s r - to retarget a section
   org-refile-targets '((nil :maxlevel . 3)
                        (org-agenda-files :maxlevel . 3))
   org-agenda-files (list "~/Dropbox/org/home.org"
                          "~/Dropbox/org/projects.org"
                          "~/.org-annotate-file.org"
                          "~/Dropbox/org/refile-beorg.org"
                          "~/Dropbox/org/Einkaufsliste.org"
                          "~/Dropbox/org/Work/Work.org")
   org-journal-dir "~/Dropbox/org/journal"
   org-journal-file-format "%Y-%m-%d"
   org-journal-date-prefix "#+TITLE: "
   org-journal-date-format "%A, %B %d %Y"
   org-journal-time-prefix "* "
   org-journal-time-format "")

  ;; additional leader key bindings for org functionality.
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "oy" #'org-copy-special
    "oc" #'org-cut-special
    "op" #'org-paste-special)
  )
