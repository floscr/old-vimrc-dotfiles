;;; -*- lexical-binding: t; -*-

(define-key evil-visual-state-map (kbd "gS") #'evil-ex-sort)

(define-key minibuffer-local-map "\C-p" 'previous-history-element)
(define-key minibuffer-local-map "\C-n" 'next-history-element)

(map! :map emacs-lisp-mode-map
      :n "g]"   #'sp-slurp-hybrid-sexp
      :n "g["   #'sp-forward-barf-sexp)

(defun paste-from-x-clipboard()
  (interactive)
  (shell-command
   (cond
    (IS-MAC "pbpaste")
    (t "xsel -ob"))
   1)
  (doom/forward-to-last-non-comment-or-eol))

(defun copy-minibuffer-line ()
  "Copies the minibuffer content to the clipboard"
  (interactive)
  (save-excursion
    (doom/forward-to-last-non-comment-or-eol)
    (set-mark-command nil)
    (doom/backward-to-bol-or-indent)
    (kill-ring-save (mark) (point))))

(defun setup-minibuffer ()
  "Set up keybindings for the minibuffer"
  (local-set-key (kbd "M-v") 'paste-from-x-clipboard)
  (local-set-key (kbd "M-c") 'copy-minibuffer-line))

(remove-hook 'minibuffer-setup-hook 'setup-minibuffer)

;; TODO For some reason this doesnt work inside the map block...
(after! evil
  (map! :n "[ SPC" #'evil-motion-insert-newline-above
        :n "] SPC" #'evil-motion-insert-newline-below))

(map!
 :niv "M-="   #'default-text-scale-increase
 :niv "M--"   #'default-text-scale-decrease
 :niv "M-0"   #'default-text-scale-reset
 :niv "M-W"   #'delete-frame
 :niv "M-X"   #'+org-capture/open-frame

 :en "C-±"   #'+popup/raise

 ;; Easier window navigation
 :en "C-h"   #'evil-window-left
 :en "C-j"   #'evil-window-down
 :en "C-k"   #'evil-window-up
 :en "C-l"   #'evil-window-right

 ;; Umlaut
 :i "A-;"   (λ! (insert "ö"))
 :i "A-:"   (λ! (insert "Ö"))
 :i "A-'"   (λ! (insert "ä"))
 :i "A-\""  (λ! (insert "Ä"))
 :i "A-["   (λ! (insert "ü"))
 :i "A-{"   (λ! (insert "Ü"))
 :i "A-s"   (λ! (insert "ß"))
 :i "A-e"   (λ! (insert "€"))
 :i "A-`"   (λ! (insert "°"))
 :i "A-."   (λ! (insert "…"))
 :i "A-l"   (λ! (insert "λ"))

 :n "gb" #'evil-switch-to-windows-last-buffer
 :n "]f" #'dumb-jump-go
 :n "[f" #'dumb-jump-back
 :n "[1" #'+MM|other-file
 :n "]1" #'+MM|other-file

 :leader
 :n "'"   #'+popup/toggle
 :n "au"   #'undo-tree-visualize
 :n "//"   #'helm-projectile-ag

 ;; Fixes for treemacs window navigation
 (:after treemacs-evil
   :n "C-h" #'evil-window-left
   :n "C-l" #'evil-window-right)

 (:desc "Toggle last iBuffer" :n "=" #'+popup/toggle)

 (:desc "search" :prefix "/"
   :desc "Search project" :n  "p" #'helm-projectile-ag)

 (:desc "toggle" :prefix "t"
   :desc "Theme Dark/Light" :n  "t" #'+doom|toggle-theme)

 (:desc "buffer" :prefix "b"
   :desc "Delete File" :n  "D" #'delete-current-buffer-file
   :desc "Delete File" :n  "O" #'doom/kill-matching-buffers
   :desc "Delete File" :n  "X" #'+doom|open-elisp-scratch-buffer)

 (:desc "git" :prefix "g"
   :desc "Checkout"   :n  "b" #'magit-checkout
   :desc "Blame"      :n  "B" #'magit-blame
   :desc "New Branch" :n  "N" #'magit-branch-spinoff)

 (:desc "window" :prefix "w"
   :desc "Split Vertical" :n "|" #'evil-window-vsplit
   :desc "Split Horizontal" :n "_" #'evil-window-split)

 (:desc "project" :prefix "p"
   :desc "services" :n  "s" #'prodigy
   :desc "Workspace Project Files" :n  "P" #'+workspace|find-workspace-project-file)
 (:desc "Yank" :prefix "y"
   :desc "filename"  :n  "f" (λ! (copy-message (file-name-nondirectory buffer-file-name)))
   :desc "base"      :n  "b" (λ! (copy-message (file-name-base (buffer-file-name))))
   :desc "directory" :n  "d" (λ! (copy-message (file-name-directory (buffer-file-name))))
   :desc "path"      :n  "p" (λ! (copy-message (file-name-directory (buffer-file-name)))))
 (:desc "workspace" :prefix [tab]
   :desc "Create" :n "c" (λ! (let* ((name (read-string "New workspace name: ")))
                               (+workspace/new name)))
   :desc "Last visited" :n "0" #'+workspace/switch-to-last-visited))
