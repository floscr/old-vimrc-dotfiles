(defconst floscr-org-packages
  '(org spacemacs-org))

(defun mfa-org/post-init-org ()
  (setq
   ;; automatically indent org sections.
   org-startup-indented t
   ;; Add files to refile targets
   ;; , s r - to retarget a section
   org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))
   org-agenda-files (list "~/Dropbox/org/home.org"
                          "~/Dropbox/org/projects.org"
                          "~/Dropbox/org/refile-beorg.org"
                          "~/Dropbox/org/Einkaufsliste.org"
                          "~/Dropbox/org/Work/Work.org"))

  ;; additional leader key bindings for org functionality.
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "oy" #'org-copy-special
    "oc" #'org-cut-special
    "op" #'org-paste-special)
  )
