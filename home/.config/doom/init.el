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

<<<<<<< HEAD
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
=======
       :ui
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-modeline     ; a snazzy Atom-inspired mode-line
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       evil-goggles      ; display visual hints when editing in evil
      ;fci               ; a `fill-column' indicator
       hl-todo           ; highlight TODO/FIXME/NOTE tags
       nav-flash         ; blink the current line after jumping
       neotree           ; a project drawer, like NERDTree for vim
      ;treemacs          ; a project drawer, like neotree but cooler
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       ;; pretty-code       ; replace bits of code with pretty symbols
       ; tabbar            ; FIXME an (incomplete) tab bar for Emacs
      ;unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
>>>>>>> bf9cf7398990738cf82fc855853bc130d3476024

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
 markdown
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
