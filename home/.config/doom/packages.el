;;; Evil Packages
;; TODO Evil-Briefcase does not work
(package! evil-briefcase :recipe (:host github :repo "strickinato/evil-briefcase"))
(package! evil-plugin :recipe (:host github :repo "tarao/evil-plugins"))
(package! evil-replace-with-register)
(package! evil-text-objects-javascript :recipe (:host github :repo "urbint/evil-text-objects-javascript"))

;;; Javascript Packages
(package! indium)

;;; Org-Mode Packages
(package! helm-org-rifle)
(package! org-pinboard :recipe (:host github :repo "floscr/org-pinboard"))
(package! poporg)
(package! org-ql :recipe (:host github :repo "alphapapa/org-ql"))
(package! org-super-agenda :recipe (:host github :repo "alphapapa/org-super-agenda"))

;;; Utils
;; Show changes in current branch
(package! git-lens)
;; Image editing utility
(package! blimp)
;; Colorized Hex Strings
(package! rainbow-mode)
;; (package! beancount :recipe
;;    (:host bitbucket :repo "blais/beancount" :files ("editors/emacs/*.el")))

(package! visual-fill-column)

(package! nov)

(package! define-word)

;; Install frame cmds
(package! frame-fns :recipe (:host wiki))
(package! frame-cmds :recipe (:host wiki))

(package! dired-recent)

;; Disabled packages
(package! lsp-ui :disable t)         ;; Annoying LSP Interface
(package! treemacs-magit :disable t) ;; Hangs on large projects
