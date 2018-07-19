;;; ~/.homesick/repos/Dotfiles/home/.config/doom/config.el -*- lexical-binding: t; -*-

;;; DEFAULTS

(setq projectile-globally-ignored-file-suffixes '(".org_archive"))
(load! "+ui")

;; JAVASCRIPT

(setq flycheck-javascript-eslint-executable (executable-find "eslint_d"))
(after! rjsx-mode (add-hook 'js2-mode-hook #'eslintd-fix-mode))

;; THEME

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
(load! "+MM")
