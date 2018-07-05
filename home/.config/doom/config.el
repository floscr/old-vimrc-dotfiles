;;; ~/.homesick/repos/Dotfiles/home/.config/doom/config.el -*- lexical-binding: t; -*-

;;; Defaults

(remove-hook 'doom-post-init-hook #'blink-cursor-mode)

;;;;;;;;;;;;
;;; BINDINGS
;;;;;;;;;;;;

(map!
 :n "M-="   #'default-text-scale-increase
 :n "M--"   #'default-text-scale-decrease
 :n "M-0"   #'default-text-scale-reset
 )

;; THEME

(setq-default line-spacing 0.15)

(setq flycheck-javascript-eslint-executable (executable-find "eslint_d"))

(setq
 ;; ScrollOff 10 lines
 scroll-conservatively 10
 scroll-margin 10)

;; Remove Scrolloff for terminal
(add-hook 'term-mode-hook (lambda () (setq-local scroll-margin 0)))

(defconst light-theme 'doom-one)
(defconst dark-theme  'doom-one-light)

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (when (and (configuration-layer/package-usedp 'projectile)
                   (projectile-project-p))
          (call-interactively #'projectile-invalidate-cache))
        (message "File '%s' successfully removed" filename)))))

(defun +doom|toggle-theme ()
  "Toggle between light and dark themes."
  (interactive)
  (cond ((eq doom-theme dark-theme)
         (message "Toggling to light-theme: %s" light-theme)
         (setq doom-theme light-theme)
         (doom/reload-theme))
        ((eq doom-theme light-theme)
         (message "Toggling to dark-theme: %s" dark-theme)
         (setq doom-theme dark-theme)
         (doom/reload-theme))
        (t (message "Toggling theme is not possible. Theme is not currently light-theme (%s) or dark-theme (%s)." light-theme dark-theme))))

;; Custom Leader Bindings

(map! :leader
      (:desc "search" :prefix "/"
        :desc "Search project" :n  "p" #'helm-projectile-ag
        )
      (:desc "toggle" :prefix "t"
        :desc "Theme Dark/Light" :n  "t" #'+doom|toggle-theme
        )
      (:desc "buffer" :prefix "b"
        :desc "Delete File" :n  "D" #'delete-current-buffer-file
        )
      )

;;; Magit

(setq-default magit-save-repository-buffers 'dontask)

;;; Undo Tree

;; Branching undo
(def-package! undo-tree
  :after-call (doom-exit-buffer-hook after-find-file)
  :config
  (setq undo-tree-auto-save-history t
      undo-tree-history-directory-alist `((".*" . ,temporary-file-directory)))
  (global-undo-tree-mode +1))

;; Replace with register

(def-package!
  evil-replace-with-register
  :config
  (setq evil-replace-with-register-key (kbd "gr"))
  (evil-replace-with-register-install)
  )

;;; Org

(setq org-directory (expand-file-name "~/Dropbox/org"))
(setq org-default-notes-file (concat org-directory "/inbox.org"))
(setq org-shopping-list (concat org-directory "/shoppinglist.org"))

(setq org-capture-templates
      (quote (("t" "todo" entry (file org-default-notes-file)
               "* TODO %?\n\t%U\n\t%a\n")
              ("c" "Chrome" entry (file+headline "~/.emacs.d/gtd.org" "Quick notes")
               "* TODO [#C] %?\n %(org-mac-chrome-get-frontmost-url)\n %i\n %U"
               :empty-lines 1)
              ("s" "shoppinglist" entry (file org-shopping-list)
               "* Supermarkt\n\t- [ ] %?")
              ("i" "idea" entry (file org-default-notes-file)
               "* %? :IDEA:\n\t%U\n\t%a\n")
              ("n" "note" entry (file org-default-notes-file)
               "* %? :NOTE:\n\t%U\n\t%a\n")
              )))

(setq org-todo-keywords
      '((sequence "TODO(t)" "SUBTREE(s)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELLED(c@/!)")
        (sequence "CRASH(c)" "BUG(b)" "REQUEST(r)" "TEST(e)" "|" "FIXED(f)")))

(setq org-todo-keyword-faces
      '(("WAIT" . "white")
        ("CRASH" . "red")
        ("BUG" . "red")
        ("SUBTREE" . "grey")
        ("TEST" . "turquoise1")
        ))

(defun +org|org-open-home-file ()
   "Open the home org file"
   (interactive)
   (find-file (concat org-directory "/home.org")))

(map! :leader
      (:desc "Notes" :prefix "n"
        :desc "Agenda" :n  "a" #'org-agenda
        :desc "Home.org" :n  "h" #'+org|org-open-home-file
        :desc "Save All Org Buffers" :n  "S" #'org-save-all-org-buffers
        ))


(after! org
  :config
  (setq org-image-actual-width 600)

  (add-to-list 'org-structure-template-alist '("e" "#+BEGIN_SRC elisp\n?\n#+END_SRC\n"))
  (add-to-list 'org-structure-template-alist '("j" "#+BEGIN_SRC js\n?\n#+END_SRC\n"))
  (add-to-list 'org-structure-template-alist '("b" "#+BEGIN_SRC bash\n?\n#+END_SRC\n"))

  (setq org-agenda-files
        (list
         "~/Dropbox/org/home.org"
         "~/Dropbox/org/inbox.org"
         "~/Dropbox/org/refile-beorg.org"
         "~/Dropbox/org/shoppinglist.org"
         "~/Dropbox/org/Work/Work.org"
         "~/Dropbox/org/cooking.org"
         "~/Dropbox/org/Projects/ideas.org"
         ))

  (setq org-agenda-refile (org-agenda-files))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  )
