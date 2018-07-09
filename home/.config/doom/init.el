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
 (company +auto)
 (helm +fuzzy)

 :ui
 doom
 doom-dashboard
 doom-modeline
 doom-quit
 hl-todo
 ;; nav-flash
 neotree
 (popup +all +defaults)
 ;; pretty-code
 vc-gutter
 vi-tilde-fringe
 window-select

 :editor
 rotate-text

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

 :lang
 data
 emacs-lisp
 javascript
 (org
  +attach
  +babel
  +capture
  +export
  +present)
 sh

 :config
 (default +bindings +snippets +evil-commands))

(provide 'init)
;;; init.el ends here
