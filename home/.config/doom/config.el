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
(load! "+workspace")

(if (getenv "ENABLE_MEISTERLABS")
    (load! "+MM"))

;; auto-mode-alist
(add-to-list 'auto-mode-alist '("Brewfile" . shell-script-mode))

;; Set the default multi-term to zsh
(setq multi-term-program "/bin/zsh")

;; Save command history
(savehist-mode 1)

;; Sort by occurance
;; https://github.com/company-mode/company-mode/issues/52
(setq company-transformers '(company-sort-by-occurrence)
      company-idle-delay 0.5)

;; Repeat snipe after further key press
(setq evil-snipe-repeat-keys t)

;; automatically reload tags files
(setq tags-revert-without-query 1)

;; Use Emacs UI to enter the encryption key
(setenv "GPG_AGENT_INFO" nil)
(setq epa-pinentry-mode 'loopback)

(put 'dired-find-alternate-file 'disabled nil)

(global-eldoc-mode -1)

;; ;; Branching undo
;; (def-package! undo-tree
;;   :after-call (doom-exit-buffer-hook after-find-file)
;;   :config
;;   (setq undo-tree-auto-save-history t
;;       undo-tree-history-directory-alist `((".*" . ,temporary-file-directory)))
;;   (global-undo-tree-mode +1))

;; Replace with register
(def-package! evil-replace-with-register
  :config
  (setq evil-replace-with-register-key (kbd "gr"))
  (evil-replace-with-register-install))

(def-package! blimp
  :config
  (add-hook 'image-mode-hook 'blimp-mode))
