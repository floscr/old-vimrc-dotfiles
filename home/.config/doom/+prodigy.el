;;; ~/.homesick/repos/dotfiles/home/.config/doom/+prodigy.el -*- lexical-binding: t; -*-

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
      :args '("start")
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
      :url "localhost:7007"
      :command "npm"
      :args '("run" "start" "meistercanvas" "--" "--port" "7007")
      :path '("~/.nvm/versions/node/v8.8.1/bin")
      :cwd "~/Code/Meisterlabs/meistercanvas")))
