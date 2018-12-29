(package! evil-replace-with-register)
(package! default-text-scale)
(package! org-journal)
(package! org-web-tools)
(package! org-super-agenda)
(package! helm-org-rifle)
(package! indium)
(package! import-js)
(package! request)
(package! git-lens)
(package! evil-plugin :recipe (:fetcher github :repo "tarao/evil-plugins"))
(package! blimp)
(package! reason-mode)
(package! rainbow-mode)
(package! org-pinboard :recipe (:fetcher github :repo "floscr/org-pinboard"))
(package! evil-matchit :recipe (:fetcher github :repo "redguardtoo/evil-matchit" :commit "7d65b4167b1f0086c2b42b3aec805e47a0d355c4"))

;; TODO Evil-Briefcase does not work
;; (package! evil-briefcase :recipe (:fetcher github :repo "strickinato/evil-briefcase"))

;; Like abolish.vim
;; Search and replace multiples
;; TODO: Add literate config here
(package! plur)

; (package! dired-subtree
;   (bind-keys :map dired-mode-map
;              (">" . dired-subtree-insert)
;              ("<" . dired-subtree-remove)))
