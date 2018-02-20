;; Org Directory
(global-set-key (kbd "C-x M-1") (lambda() (interactive)(find-file "~/Dropbox/org/")))

(with-eval-after-load 'org
  (setq org-agenda-files '("~/Dropbox/org/"))
  (setq org-agenda-custom-commands
        '(("c" "Simple agenda view"
           ((agenda "")
            (alltodo "")))))

  (setq org-publish-project-alist
        '(("org"
           :base-directory "~/Dropbox/org/"
           :publishing-directory "~/Dropbox/org/html"
           :publishing-function org-html-publish-to-html
           :section-numbers nil
           :with-toc nil
           :html-head "<link rel=\"stylesheet\"
                    href=\"solarized-light.css\"
                    type=\"text/css\"/>"
           :auto-sitemap t                ; Generate sitemap.org automagically...
           :sitemap-filename "index.org"
           :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
           )))

  (org-defkey org-mode-map [(meta return)] 'org-meta-return)

  )
