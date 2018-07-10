;;; -*- lexical-binding: t; -*-

(map!
 :niv "M-="   #'default-text-scale-increase
 :niv "M--"   #'default-text-scale-decrease
 :niv "M-0"   #'default-text-scale-reset

 :leader
 :n "ss"   #'helm-swoop
 )


