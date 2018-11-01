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
(load! "+reason")

(if (getenv "ENABLE_MEISTERLABS")
    (load! "+MM"))

(defalias 'λ 'lambda)

(after! prodigy
  (define-key prodigy-mode-map "q" #'doom/escape)
  (define-key prodigy-mode-map "j" #'prodigy-next)
  (define-key prodigy-mode-map "k" #'prodigy-prev)
  (define-key prodigy-mode-map "G" #'prodigy-last)
  (let ((external-url (shell-command-to-string "echo -n $(ifconfig en0 | awk '$1 == \"inet\" {print \"http://\" $2}'):3001")))
    (prodigy-define-service
      :name "mindmeister-web"
      :url "localhost:3000"
      :command "npm"
      :args '("run" "start" "PrivateMaps")
      :path '("~/.nvm/versions/node/v8.8.1/bin")
      :cwd "~/Code/Meisterlabs/mindmeister-web"
      :tags '(mindmeister frontend))
    (prodigy-define-service
      :name "mindmeister-web production"
      :command "npm"
      :url external-url
      :args (list "run" "start" "PrivateMaps" "--" "--production" "--mmEndpoint" external-url)
      :path '("~/.nvm/versions/node/v8.8.1/bin")
      :cwd "~/Code/Meisterlabs/mindmeister-web"
      :tags '(mindmeister frontend production))
    (prodigy-define-service
      :name "mindmeister"
      :url "localhost:3001"
      :command "rails"
      :args '("s" "-p" "3000")
      :cwd "~/Code/Meisterlabs/mindmeister")
    (prodigy-define-service
      :name "meistercanvas"
      :url "localhost:7000"
      :command "npm"
      :args '("run" "start" "meistercanvas" "--" "--port" "7000")
      :path '("~/.nvm/versions/node/v8.8.1/bin")
      :cwd "~/Code/Meisterlabs/meistercanvas")))

(setq +lookup-provider-url-alist
  '(("Google"            . "https://google.com/search?q=%s")
    ("Google images"     . "https://google.com/images?q=%s")
    ("Google maps"       . "https://maps.google.com/maps?q=%s")
    ("Hoogle"            . "https://www.haskell.org/hoogle/?hoogle=%s")
    ("Project Gutenberg" . "http://www.gutenberg.org/ebooks/search/?query=%s")
    ("DuckDuckGo"        . "https://duckduckgo.com/?q=%s")
    ("DevDocs.io"        . "https://devdocs.io/#q=%s")
    ("StackOverflow"     . "https://stackoverflow.com/search?q=%s")
    ("Github"            . "https://github.com/search?ref=simplesearch&q=%s")
    ("Github Code"       . "https://github.com/search?search&q=%s&type=Code")
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
