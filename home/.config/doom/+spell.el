;;; ~/.homesick/repos/Dotfiles/home/.config/doom/+spell.el -*- lexical-binding: t; -*-
;; (setenv
;;   "DICPATH"
;;   (concat (getenv "HOME") "/Library/Spelling"))

;; (setq ispell-program-name "hunspell"
;;       ispell-dictionary "en_US")

(after! flyspell
  (cond
   ((executable-find "hunspell")
    (setq ispell-program-name "hunspell"
          ispell-local-dictionary "en_US"
          ispell-really-hunspell t
          ispell-local-dictionary-alist
                  ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
                  ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
                  '(("english" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "personal,en_US") nil utf-8)
                    ("german"  "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "personal,de_AT") nil utf-8))))))
