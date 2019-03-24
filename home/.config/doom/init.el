;;; init.el --- description -*- lexical-binding: t; -*-

(doom!
 :feature
 eval
 (evil +everywhere)
 file-templates
 (lookup +devdocs +docsets)
 snippets

 workspaces

 :completion
 (company
  +childframe)
 (ivy
  +fuzzy
  +childframe)

 :ui
 doom
 doom-dashboard
 modeline
 doom-quit
 hl-todo
 ;; nav-flash
 ;; neotree
 treemacs
 (popup +all +defaults)
 ;; pretty-code
 vc-gutter
 vi-tilde-fringe
 window-select

 :editor
 rotate-text
 multiple-cursors
 parinfer

 :emacs
 dired
 ediff
 electric
 eshell
 imenu
 term
 vc

 :tools
 editorconfig
 macos
 magit
 prodigy
 rgb
 pdf
 flyspell
 (flycheck +childframe)

 :lang
 data
 emacs-lisp
 javascript
 purescript
 markdown
 ocaml
 haskell
 (org
  +attach
  +babel
  +capture
  +export
  +present)
 sh

 :app
 irc
 writeme
 calendar

 :config
 (default +bindings +snippets +evil-commands)
 literate)

(provide 'init)
;;; init.el ends here

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; * Config
(setq
 insert-directory-program "gls"
 user-mail-address "flo.schroedl@gmail.com"
 user-full-name "Florian Schrödl"
 max-specpdl-size 10000)
