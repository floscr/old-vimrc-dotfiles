;;; -*- lexical-binding: t; -*-

(map!
 :niv "M-="   #'default-text-scale-increase
 :niv "M--"   #'default-text-scale-decrease
 :niv "M-0"   #'default-text-scale-reset

 :n "[1" #'+MM|other-file
 :n "]1" #'+MM|other-file

 :leader
 :n "ss"   #'helm-swoop
 )


