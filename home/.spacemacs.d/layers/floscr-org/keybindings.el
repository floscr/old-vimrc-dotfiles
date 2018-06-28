;; Annotate
(spacemacs/set-leader-keys "aof" 'org-annotate-file)

;; Rifle
(spacemacs/set-leader-keys "hr" 'nil)
(spacemacs/set-leader-keys "hrr" 'helm-org-rifle)
(spacemacs/set-leader-keys "hrc" 'helm-org-rifle-current-buffer)
(spacemacs/set-leader-keys "hrd" 'helmrorg-rifle-directories)
(spacemacs/set-leader-keys "hrf" 'helm-org-rifle-files)
;; (define-key org-mode (kbd ", s d") 'org-cut-subtree)

;; General
(spacemacs/set-leader-keys "aoS" 'org-save-all-org-buffers)

(evil-leader/set-key
  "oa" 'org-agenda
  "oh" 'floscr-org/org-open-home-file
  "od" 'floscr-org/org-agenda-day
  "oc" 'org-capture
  "ol" 'org-store-link
  "oj" 'org-journal-new-entry
  )

(evil-define-key 'normal 'org-mode "]f" 'org-journal-open-next-entry)
(evil-define-key 'normal 'org-mode "[f" 'org-journal-open-previous-entry)
