;;; Evil Packages
;; TODO Evil-Briefcase does not work
(package! evil-briefcase :recipe (:fetcher github :repo "strickinato/evil-briefcase"))
(package! evil-plugin :recipe (:fetcher github :repo "tarao/evil-plugins"))
(package! evil-replace-with-register)
(package! evil-text-objects-javascript :recipe (:fetcher github :repo "urbint/evil-text-objects-javascript"))
;; Like abolish.vim
;; Search and replace multiples
;; TODO: Add literate config here
(package! plur)

;;; Javascript Packages
(package! indium)
(package! import-js)

;;; Reasonml Packages
(package! reason-mode)

;;; Org-Mode Packages
(package! helm-org-rifle)

;;; Utils
(package! default-text-scale)
(package! request)
(package! git-lens)
;; Image editing utility
(package! blimp)

(package! rainbow-mode)
(package! org-pinboard :recipe (:fetcher github :repo "floscr/org-pinboard"))
(package! github-review :recipe (:fetcher github :repo "charignon/github-review"))

(package! beancount :recipe
   (:fetcher bitbucket :repo "blais/beancount" :files ("editors/emacs/*.el")))

(package! nov)

(package! define-word)

;; Install frame cmds
(package! frame-fns :recipe (:fetcher wiki))
(package! frame-cmds :recipe (:fetcher wiki))


(package! dired-recent)

;; Disabled packages
(package! lsp-ui :disable t)         ;; Annoying LSP Interface
(package! treemacs-magit :disable t) ;; Hangs on large projects
