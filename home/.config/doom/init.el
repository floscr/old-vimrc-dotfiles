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
 (lsp
   +ocaml)

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
 rgb

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
 org-private
 sh

 :app
 irc
 writeme

 :config
 (default +bindings +snippets +evil-commands))

(provide 'init)
;;; init.el ends here

(setq
 indicate-empty-lines nil
 which-key-idle-delay 0.3)

;; * Config
(setq
 insert-directory-program "gls"
 user-mail-address "flo.schroedl@gmail.com"
 user-full-name "Florian Schrödl"
 max-specpdl-size 10000)

(setq
 doom-localleader-key ","
 doom-localleader-key ","
 +default-repeat-forward-key ";"
 +default-repeat-backward-key "'")
