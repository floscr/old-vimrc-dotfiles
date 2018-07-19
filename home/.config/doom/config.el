;;; ~/.homesick/repos/Dotfiles/home/.config/doom/config.el -*- lexical-binding: t; -*-

;;; DEFAULTS

(setq projectile-globally-ignored-file-suffixes '(".org_archive"))

;; JAVASCRIPT

(setq flycheck-javascript-eslint-executable (executable-find "eslint_d"))
(after! rjsx-mode (add-hook 'js2-mode-hook #'eslintd-fix-mode))

;; THEME

(remove-hook 'doom-init-ui-hook #'blink-cursor-mode)
(blink-cursor-mode -1)

(cond
 ((string= system-name "Florians-iMac.local")
  (setq-default line-spacing 0.3)
  )
 (t
  (setq-default line-spacing 0.15)
  ))

(setq
 scroll-conservatively 10
 scroll-margin 10)

;; Remove Scrolloff for terminal
(add-hook 'term-mode-hook (λ! (setq-local scroll-margin 0)))

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

(defun +MM|other-file ()
  "Toggle between component or controller"
  (interactive)
  (setq filename (file-name-nondirectory buffer-file-name))
  (setq path (file-name-directory buffer-file-name))
  (setq target (if (string= filename "component.js") "controller.js" "component.js"))
  (find-file (concat path target))
  )

(fset '+MM|turn-style-object-into-function
   (lambda (&optional arg)
     "Turns an object into a Style function, needs to be focused on the starting {"
     (interactive "p")
     (kmacro-exec-ring-item (quote ([?y ?s ?a ?B ?b ?i ?S ?t ?y ?l ?e escape ?l ?a ?f ?u ?n ?c ?t ?i ?o ?n ?  S-backspace ?  ?\( ?o ?p ?t ?i ?o ?n ?s ?, ?  ?R ?u ?l ?e ?s escape ?l ?l ?y ?s ?a ?B ?B ?i ?  escape ?l ?a return ?r ?e ?t ?u ?r ?n ?  escape ?l ?j ?> ?i ?\{ ?k ?$ ?% ?a return escape ?k ?a ?\; escape ?= ?= ?j ?b ?l ?%] 0 "%d")) arg)
     ))

(defconst light-theme 'doom-one)
(defconst dark-theme  'doom-one-light)

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

(map! :leader (:desc "Toggle last iBuffer" :n "=" #'+popup/toggle))

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
      (:desc "git" :prefix "g"
        :desc "Checkout" :n  "b" #'magit-checkout
        :desc "Blame" :n  "B" #'magit-blame
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

(load! "+org")
(load! "+bindings")
(load! "+eldoc")
(load! "+indium")
