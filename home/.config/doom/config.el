;;; ~/.homesick/repos/Dotfiles/home/.config/doom/config.el -*- lexical-binding: t; -*-

;; Base
(load! "+macros")
(load! "+utils")
(load! "+ui")
(load! "+bindings")

;; Packages
(load! "+dired")
(load! "+eldoc")
(load! "+git")
(load! "+helm")
(load! "+indium")
(load! "+js")
(load! "+reason")
(load! "+workspace")
(load! "+prodigy")
(load! "+evil")
(load! "+spell")

(if (getenv "ENABLE_MEISTERLABS")
    (load! "+MM"))

(setq
 trash-directory "~/.Trash/"
 delete-by-moving-to-trash t)

;; Always create workspace when switching to project
(setq +workspaces-on-switch-project-behavior t)

(setq +lookup-provider-url-alist
  '(("DuckDuckGo"        . "https://duckduckgo.com/?q=%s")
    ("Github Code"       . "https://github.com/search?search&q=%s&type=Code")
    ("Google"            . "https://google.com/search?q=%s")
    ("Google images"     . "https://google.com/images?q=%s")
    ("Google maps"       . "https://maps.google.com/maps?q=%s")
    ("NPM"               . "https://npmjs.com/search?q=%s")
    ("Hoogle"            . "https://www.haskell.org/hoogle/?hoogle=%s")
    ("Project Gutenberg" . "http://www.gutenberg.org/ebooks/search/?query=%s")
    ("DevDocs.io"        . "https://devdocs.io/#q=%s")
    ("StackOverflow"     . "https://stackoverflow.com/search?q=%s")
    ("Github"            . "https://github.com/search?ref=simplesearch&q=%s")
    ("Youtube"           . "https://youtube.com/results?aq=f&oq=&search_query=%s")
    ("Wolfram alpha"     . "https://wolframalpha.com/input/?i=%s")
    ("Wikipedia"         . "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")))

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

(after! smerge-mode
  :config
  ;; TODO This is broken after switching the theme but works for now
  ;; This fixes the smerge diff color is really bright an ugly
  (set-face-attribute 'smerge-refined-added nil :foreground nil :background nil))
