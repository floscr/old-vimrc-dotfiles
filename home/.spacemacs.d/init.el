(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   dotspacemacs-install-packages 'used-only
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-ask-for-lazy-installation nil
   dotspacemacs-enable-lazy-installation 'unused

   dotspacemacs-configuration-layers
   '(
     ;; Syntax
     (javascript :variables node-add-modules-path t)
     html
     yaml
     vimscript
     markdown
     ruby
     reason

     (ranger :variables
             ranger-show-preview t)

     floscr-git
     floscr-defaults
     floscr-org
     tabbar

     helm
     osx
     spacemacs-org
     org

     ;; Version Control
     (version-control :variables
                      version-control-global-margin t
                      version-control-diff-tool     'git-gutter
                      )
     git

     ;; Emacs
     (auto-completion
      :variables
      auto-completion-enable-sort-by-usage t
      auto-completion-idle-delay 0.05
      auto-completion-enable-snippets-in-popup t
      auto-completion-tab-key-behavior 'cycle
      auto-completion-private-snippets-directory "~/.spacemacs.d/snippets/"
      :disabled-for org spacemacs-org)
     neotree

     ;; Terminal
     (shell :variables
            shell-default-shell 'ansi-term
            shell-default-term-shell "/bin/zsh"
            shell-default-height 30
            shell-default-position 'bottom)

     syntax-checking
     ;; spell-checking
     )

   dotspacemacs-additional-packages '(
                                      editorconfig
                                      (rjsx-mode :location (recipe :fetcher github :repo "floscr/rjsx-mode"))
                                      evil-replace-with-register
                                      eslintd-fix
                                      helm-ls-git
                                      helm-org-rifle
                                      org-preview-html
                                      )

   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '(evil-escape
                                    vi-tilde-fringe)
   ))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  (setq-default
   ;; Packages
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-elpa-subdirectory 'emacs-version

   dotspacemacs-editing-style 'vim
   dotspacemacs-folding-method 'evil
   dotspacemacs-loading-progress-bar nil

   ;; Startup Buffer
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   dotspacemacs-startup-banner 'nil
   dotspacemacs-startup-buffer-responsive nil

   ;; Scratch Buffer
   dotspacemacs-scratch-mode 'elisp-mode
   dotspacemacs-initial-scratch-message nil

   ;; Themes
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-mode-line-theme '(spacemacs :separator slant :separator-scale 1.5)

   ;; Leader Key
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ t

   ;; Visual Mode tab shifting
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text t
   dotspacemacs-ex-substitute-global t

   ;; Layout
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts t
   dotspacemacs-auto-generate-layout-names nil
   dotspacemacs-switch-to-buffer-prefers-purpose t

   ;; Fullscreen
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil

   ;; Disable smooth scrolling as its super slow
   dotspacemacs-smooth-scrolling nil

   ;; File Handling
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 10

   ;; Helm
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header t
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always

   ;; Which Key
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom

   ;; Transient State
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols nil
   ;; If non-nil, the paste transient-state is enabled. While enabled, pressing
   ;; `p' several times cycles through the elements in the `kill-ring'.
   dotspacemacs-enable-paste-transient-state t

   dotspacemacs-enable-server t
   dotspacemacs-persistent-server nil
   dotspacemacs-zone-out-when-idle nil

   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   dotspacemacs-frame-title-format nil
   dotspacemacs-icon-title-format nil

   dotspacemacs-whitespace-cleanup 'trailing

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  (setq-default line-spacing 0.15)
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  (global-company-mode t)

  ;; Automatic load changes
  (global-auto-revert-mode t)

  ;; Keep history
  (setq undo-tree-auto-save-history t)

  ;; Persistent undo-tree
  (require 'undo-tree)
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/undo-tree/"))))

  (setq-default magit-save-repository-buffers 'dontask)

  ;; Evil Replace Motion
  (setq evil-replace-with-register-key (kbd "gr"))
  (evil-replace-with-register-install)

  ;; Make the fringe background same as the linenumber
  (set-face-attribute 'fringe nil :background (face-attribute 'mode-line :background))

  (add-to-list 'exec-path "~/.nvm/versions/node/v8.8.1/bin")

  ;; Hide errors for js2
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)

  ;; Transparent titlebar
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

  (when (member "Symbol" (font-family-list))
    (set-fontset-font t 'unicode "Symbol" nil 'prepend))

  ;; Speed up projectile
  (setq projectile-enable-caching t)
  (setq shell-file-name "/bin/bash")

  ;; Hide the title
  (setq frame-title-format "")

  ;; Use Emacs UI to enter the encryption key
  (setq epa-pinentry-mode 'loopback)

  (add-hook 'text-mode-hook 'display-line-numbers-mode)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)

  (eval-after-load 'rjsx-mode
    '(progn (add-hook 'rjsx-mode-hook #'add-node-modules-path)))

  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

  (with-eval-after-load 'flycheck
    (push 'javascript-jshint flycheck-disabled-checkers)
    (push 'json-jsonlint flycheck-disabled-checkers))
  (spacemacs/enable-flycheck 'rjsx-mode)

  (setq node-add-modules-path t)
  (add-hook 'rjsx-mode-hook 'eslintd-fix-mode)

  (eval-after-load 'js-mode
    '(add-hook 'rjsx-mode-hook #'add-node-modules-path))

  (setq ns-use-srgb-colorspace nil)
  (setq powerline-image-apple-rgb nil)

  (setq powerline-default-separator 'slant)

  (setq flycheck-javascript-eslint-executable (executable-find "eslint_d"))

  (use-package flycheck
    :ensure t
    :config
    (add-hook 'after-init-hook 'global-flycheck-mode)
    (add-hook 'flycheck-mode-hook #'add-node-modules-path)
    (setq-default flycheck-highlighting-mode 'lines)
    ;; Define fringe indicator / warning levels
    (define-fringe-bitmap 'flycheck-fringe-bitmap-ball
      (vector #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00011100
              #b00111110
              #b00111110
              #b00111110
              #b00011100
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000))
    (flycheck-define-error-level 'error
      :severity 2
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap 'flycheck-fringe-bitmap-ball
      :fringe-face 'flycheck-fringe-error)
    (flycheck-define-error-level 'warning
      :severity 1
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap 'flycheck-fringe-bitmap-ball
      :fringe-face 'flycheck-fringe-warning)
    (flycheck-define-error-level 'info
      :severity 0
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap 'flycheck-fringe-bitmap-ball
      :fringe-face 'flycheck-fringe-info))

  ;; Enable Gitgutter with fringe
  (use-package git-gutter-fringe
    :ensure t
    :hook (after-init . global-git-gutter-mode)
    :config
    (require 'git-gutter)
    (setq git-gutter-fr:side 'left-fringe
          git-gutter:separator-sign "|"
          git-gutter:lighter ""
          git-gutter:update-interval 1)
    (set-face-foreground  'git-gutter:separator "yellow")
    (fringe-helper-define 'git-gutter-fr:modified nil
      "X"
      "X"
      "X"
      "X"
      "X"
      "X"
      "X"
      "X"))

  ;;; scroll one line at a time (less "jumpy" than defaults)
  (setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ;; two lines at a time
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

  (setq
   ;; ScrollOff 10 lines
   scroll-conservatively 10
   scroll-margin 10)
  )


(setq custom-file "~/.emacs.d/.cache/.custom-settings")
(load custom-file)
