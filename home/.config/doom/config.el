;;; ~/.homesick/repos/Dotfiles/home/.config/doom/config.el -*- lexical-binding: t; -*-

(load! "+ui")
(load! "+utils")
(load! "+git")
(load! "+js")
(load! "+org")
(load! "+bindings")
(load! "+eldoc")
(load! "+indium")
(load! "+dired")
(load! "+helm")

(if (getenv "ENABLE_MEISTERLABS")
    (load! "+MM"))

;; ETC / TEMP

;; Set the default multi-term to zsh
(setq multi-term-program "/bin/zsh")

;; Use Emacs UI to enter the encryption key
(setenv "GPG_AGENT_INFO" nil)
(setq epa-pinentry-mode 'loopback)

(put 'dired-find-alternate-file 'disabled nil)

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

