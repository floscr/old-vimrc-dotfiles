;;; init.el --- description -*- lexical-binding: t; -*-

(doom!
 :feature
 eval
 (evil +everywhere)
 file-templates
 (lookup +devdocs +docsets)
 snippets
 spellcheck
 (syntax-checker +childframe)
 workspaces

 :completion
 (company
  +auto
  +childframe)
 (helm +fuzzy)

 :ui
 doom
 doom-dashboard
 doom-modeline
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

 :config
 (default +bindings +snippets +evil-commands))

(provide 'init)
;;; init.el ends here
