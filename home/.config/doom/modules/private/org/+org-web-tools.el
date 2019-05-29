;;; lang/+org/+org-web-tools.el -*- lexical-binding: t; -*-

(def-package! org-web-tools
  :after org
  :command (+org-web-tools/read-url-at-point))
