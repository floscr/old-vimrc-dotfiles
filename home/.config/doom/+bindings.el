;;; -*- lexical-binding: t; -*-

(map!
 :niv "M-="   #'default-text-scale-increase
 :niv "M--"   #'default-text-scale-decrease
 :niv "M-0"   #'default-text-scale-reset

 ;; Umlaut
 :i "A-;"   (λ! (insert "ö"))
 :i "A-:"   (λ! (insert "Ö"))
 :i "A-'"   (λ! (insert "ä"))
 :i "A-\""  (λ! (insert "Ä"))
 :i "A-["   (λ! (insert "ü"))
 :i "A-{"   (λ! (insert "Ü"))
 :i "A-s"   (λ! (insert "ß"))
 :i "A-e"   (λ! (insert "€"))

 :n "gb" #'evil-switch-to-windows-last-buffer
 :n "[1" #'+MM|other-file
 :n "]1" #'+MM|other-file

 :leader
 :n "'"   #'+popup/toggle
 :n "ss"   #'helm-swoop
 :n "au"   #'undo-tree-visualize
 :n "//"   #'helm-projectile-ag

 (:desc "Toggle last iBuffer" :n "=" #'+popup/toggle)

 (:desc "search" :prefix "/"
   :desc "Search project" :n  "p" #'helm-projectile-ag
   )
 (:desc "toggle" :prefix "t"
   :desc "Theme Dark/Light" :n  "t" #'+doom|toggle-theme
   )
 (:desc "buffer" :prefix "b"
   :desc "Delete File" :n  "D" #'delete-current-buffer-file
   )
 (:desc "git" :prefix "g"
   :desc "Checkout" :n  "b" #'magit-checkout
   :desc "Blame" :n  "B" #'magit-blame
   )
 )
