;;; lang/+org/+org-web-tools.el -*- lexical-binding: t; -*-

(defun +org-web-tools/dwim-at-point ()
  "Pass url to web tools from either:
1. An org link under the cursor
2. An url in the clipboard"
  (interactive)
  (let ((org-url (org-element-property :raw-link (org-element-context)))
        (clipboard-url (current-kill 0)))
    (if org-url
        (message "Reading org url from thing at point")
      (org-web-tools-read-url-as-org org-url)
      (if (string-match url-handler-regexp clipboard-url)
          (message "Reading org url from clipboard")
        (org-web-tools-read-url-as-org clipboard-url)
        (message "No url found")))))

(defun +org-web-tools/read-url-at-point ()
  "Open the url under the cursor"
  (interactive)
  (org-web-tools-read-url-as-org (org-web-tools--read-url))
  (visual-line-mode)
  (visual-fill-column-mode)
  (setq display-line-numbers nil))

(def-package! org-web-tools
  :after org
  :commands (+org-web-tools/read-url-at-point))
