(defun org-day-agenda ()
  (let ((org-agenda-prefix-format "  %?-12t% s")
        (org-agenda-start-on-weekday nil)
        (org-agenda-span 1)
        (org-agenda-start-day ".")
        (org-agenda-skip-scheduled-if-done t)
        (org-agenda-sorting-strategy '(timestamp-down))
        (org-super-agenda-groups '((:name "Today" :date today :time-grid t)
                                   (:name "Overdue" :scheduled past)
                                   (:name "Future" :anything (:scheduled future)))))
    (org-agenda nil "a")))


(let ((org-agenda-prefix-format "  %?-12t% s")
      (org-agenda-sorting-strategy '(timestamp-down))
      (org-agenda-files (--map (concat org-directory "/" it) '("inbox.org" "home.org")))
      (org-super-agenda-groups '((:name "Next" :todo ("ACTIVE" "NEXT"))
                                 (:name "Scheduled" :scheduled t)
                                 (:name "Unscheduled" :and (:todo "TODO" :scheduled nil)))))
  (org-agenda nil "t"))
