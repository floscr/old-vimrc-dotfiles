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

 :n "gb" #'evil-switch-to-windows-last-buffer
 :n "[1" #'+MM|other-file
 :n "]1" #'+MM|other-file

 :leader
 :n "ss"   #'helm-swoop
 :n "au"   #'undo-tree-visualize
 )
