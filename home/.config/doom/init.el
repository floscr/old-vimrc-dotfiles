;;; init.el --- description -*- lexical-binding: t; -*-
(doom!
 :completion
 (company
  +childframe)
 (ivy
  +fuzzy
  +childframe)

 :ui
 doom
 modeline
 doom-quit
 hl-todo
 treemacs
 (popup +all +defaults)
 vc-gutter
 vi-tilde-fringe
 window-select
 workspaces

 :editor
 (evil +everywhere)
 file-templates
 fold
 rotate-text
 multiple-cursors
 parinfer
 snippets

 :term
 eshell
 term

 :emacs
 dired
 electric
 vc

 :tools
 (lookup +devdocs +docsets)
 eval
 editorconfig
 macos
 magit
 rgb
 pdf
 flyspell
 (flycheck +childframe)
 lsp

 :lang
 data
 emacs-lisp
 (javascript +lsp)
 purescript
 markdown
 ocaml
 haskell
 (org
  +dragndrop
  +present)
 sh

 :app
 irc
 writeme
 calendar

 :config
 (default +bindings +snippets +evil-commands)
 literate

 :private
 reason
 work
 (org
  +org-web-tools
  +org-reading-list))

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
