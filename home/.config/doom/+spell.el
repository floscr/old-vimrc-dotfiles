;;; ~/.homesick/repos/Dotfiles/home/.config/doom/+spell.el -*- lexical-binding: t; -*-

(after! flyspell
  (cond
   ((executable-find "hunspell")

    ;; For the switching, "german" has to be also in this alist
    (add-to-list 'ispell-hunspell-dict-paths-alist (list "german" (expand-file-name "~/Library/Spelling/de_AT.aff")))

    (setq ispell-program-name "hunspell"
          ispell-local-dictionary "en_US"
          ispell-really-hunspell t
          ispell-local-dictionary-alist
                  ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
                  ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
                  '(("english" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "personal,en_US") nil utf-8)
                    ("german"  "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "personal,de_AT") nil utf-8))))))

(defun flyspell-set-language-environment ()
  "Change flyspell language based on the language environment"
  (cond
   ((string= "English" current-language-environment)
    (setq ispell-local-dictionary "english"))
   ((string= "German" current-language-environment)
    (setq ispell-local-dictionary "german"))))

(add-hook 'set-language-environment-hook 'flyspell-set-language-environment)
