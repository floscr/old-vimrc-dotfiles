(defalias 'λ 'lambda)

(defmacro template (text)
  "Expand text like \"Hello <<name>>\" to (format \"Hello %s\" name)."
  (let ((pattern "<<\\(.*?\\)>>"))
    ;; The regexp matches anything between delimiters, non-greedily
    (with-temp-buffer
      (save-excursion (insert text))
      (let ((matches '()))
        (while (re-search-forward pattern nil t)
          (push (match-string 1) matches)
          (replace-match "%s" t t))
        `(format ,(buffer-string) ,@(reverse (mapcar 'read matches)))))))

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (when (and (configuration-layer/package-usedp 'projectile)
                   (projectile-project-p))
          (call-interactively #'projectile-invalidate-cache))
        (message "File '%s' successfully removed" filename)))))

(defun do-jxa-script (cmd)
  "Run a osx javascript automation script via bash"
  (shell-command-to-string
   (concat "osascript -l 'JavaScript' -e '" cmd "'")))

;; source: https://emacsredux.com/blog/2013/06/21/eval-and-replace/
(defun eval-and-replace-sexp ()
  "Replace the preceding sexp with its value."
  (interactive)
  (right-char) ;; Fix for normal mode
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun copy-message (x)
  (kill-new x)
  (message "Copied to clipboard: %s" x))

(defun +doom|open-elisp-scratch-buffer ()
  "Opens a new scratch buffer in elisp mode"
  (interactive)
  (doom/open-scratch-buffer)
  (emacs-lisp-mode))

(defun math-on-number (f &optional num)
  "Read user input and apply with function f to the number at point"
  (let* ((x (thing-at-point 'number))
         (arithmetic-symbol (pcase f
                              ('+ "+")
                              ('- "-")
                              ('/ "/")
                              ('* "*")
                              (_ (error "Unknown function %s" f))))
         (readline (concat (number-to-string x) " " arithmetic-symbol " "))
         (y (or num (read-number readline)))
         (result (funcall f x y))
         (bounds (bounds-of-thing-at-point 'evil-WORD)))
    (delete-region (car bounds) (cdr bounds))
    (insert (format "%.02f" result))))

(defun math+|add-to-number ()
  (interactive)
  (math-on-number '+))

(defun math+|subtract-from-number ()
  (interactive)
  (math-on-number '-))

(defun math+|subtract-maran-vegan ()
  (interactive)
  (math-on-number '- 8.60))

(defun math+|divide-by-number ()
  (interactive)
  (math-on-number '/))

(defun math+|multiply-by-number ()
  (interactive)
  (math-on-number '*))

(defun +workspace/switch-to-last-visited ()
  "Switch to the last visited workspace."
  (interactive)
  (+workspace/switch-to +workspace--last))

(defun +workspace|find-workspace-project-file ()
  (interactive)
  (cl-letf (((symbol-function 'projectile-project-root) #'find-workspace-project-root))
      (projectile-find-file)))

(defun find-workspace-project-root (&optional arg)
  "Gets the root dir for the current workspace"
  (--find (s-match (concat (+workspace-current-name) "/$") it) projectile-known-projects))

(remove-hook 'doom-init-ui-hook #'blink-cursor-mode)
(blink-cursor-mode -1)

(cond
 ((string= system-name "Florians-MBP.localdomain")
  (setq-default line-spacing 0.3))
 ((string= system-name "Florians-iMac.local")
  (setq-default line-spacing 0.3))
 ((string= system-name "Florians-MacBook-Air.local")
  (setq-default line-spacing 0.4)
  (setq initial-frame-alist
        (append (list '(left . 272)
                      '(width . 165)
                      '(fullscreen . fullheight))
                initial-frame-alist))
  (after! helm-mode
    :config
    ;; Fix for small helm ui on small display
    (set-popup-rule! "^\\*helm" :vslot -100 :size 0.32 :ttl nil)))
 (t (setq-default line-spacing 0.15)))

(setq
 scroll-conservatively 10
 scroll-margin 10)

;; Remove Scrolloff for terminal
(add-hook 'term-mode-hook (λ! (setq-local scroll-margin 0)))

(defconst light-theme 'doom-one)
(defconst dark-theme  'doom-one-light)

(defun +doom|toggle-theme ()
  "Toggle between light and dark themes."
  (interactive)
  (cond ((eq doom-theme dark-theme)
         (message "Toggling to light-theme: %s" light-theme)
         (setq doom-theme light-theme)
         (doom/reload-theme))
        ((eq doom-theme light-theme)
         (message "Toggling to dark-theme: %s" dark-theme)
         (setq doom-theme dark-theme)
         (doom/reload-theme))
        (t (message "Toggling theme is not possible. Theme is not currently light-theme (%s) or dark-theme (%s)." light-theme dark-theme))))

(defvar default-line-spacing 0.2)

(defun set-line-spacing (&optional spacing)
  "Set the line spacing
When no line spacing is given is the default-line-spacing"
  (if line-spacing
      (setq-default line-spacing (+ (or spacing default-line-spacing) line-spacing))
    (setq-default line-spacing (+ 0 default-line-spacing))))

(defun +ui|reset-line-spacing ()
  (interactive)
  (setq-default line-spacing nil))

(defun +ui|increase-line-spacing ()
  (interactive)
  (set-line-spacing))

(defun +ui|decrease-line-spacing ()
  (interactive)
  (set-line-spacing (- default-line-spacing)))

(define-key evil-visual-state-map (kbd "gS") #'evil-ex-sort)

(define-key minibuffer-local-map "\C-p" 'previous-history-element)
(define-key minibuffer-local-map "\C-n" 'next-history-element)

(setq
 +default-repeat-forward-key ";"
 +default-repeat-backward-key "'")

(map! :map emacs-lisp-mode-map
      :n "g]"   #'sp-slurp-hybrid-sexp
      :n "g["   #'sp-forward-barf-sexp)

(defun paste-from-x-clipboard()
  (interactive)
  (shell-command
   (cond
    (IS-MAC "pbpaste")
    (t "xsel -ob"))
   1)
  (doom/forward-to-last-non-comment-or-eol))

(defun copy-minibuffer-line ()
  "Copies the minibuffer content to the clipboard"
  (interactive)
  (save-excursion
    (doom/forward-to-last-non-comment-or-eol)
    (set-mark-command nil)
    (doom/backward-to-bol-or-indent)
    (kill-ring-save (mark) (point))))

(defun setup-minibuffer ()
  "Set up keybindings for the minibuffer"
  (local-set-key (kbd "M-v") 'paste-from-x-clipboard)
  (local-set-key (kbd "M-c") 'copy-minibuffer-line))

(remove-hook 'minibuffer-setup-hook 'setup-minibuffer)

;; TODO For some reason this doesnt work inside the map block...
(after! evil
  (map! :n "[ SPC" #'evil-motion-insert-newline-above
        :n "] SPC" #'evil-motion-insert-newline-below))

(map!
 :niv "M-="   #'default-text-scale-increase
 :niv "M--"   #'default-text-scale-decrease
 :niv "M-0"   #'default-text-scale-reset
 :niv "M-W"   #'delete-frame
 :niv "M-X"   #'+org-capture/open-frame

 :en "C-±"   #'+popup/raise

 ;; Easier window navigation
 :en "C-h"   #'evil-window-left
 :en "C-j"   #'evil-window-down
 :en "C-k"   #'evil-window-up
 :en "C-l"   #'evil-window-right

 ;; Umlaut
 :i "A-;"   (λ! (insert "ö"))
 :i "A-:"   (λ! (insert "Ö"))
 :i "A-'"   (λ! (insert "ä"))
 :i "A-\""  (λ! (insert "Ä"))
 :i "A-["   (λ! (insert "ü"))
 :i "A-{"   (λ! (insert "Ü"))
 :i "A-s"   (λ! (insert "ß"))
 :i "A-e"   (λ! (insert "€"))
 :i "A-`"   (λ! (insert "°"))
 :i "A-."   (λ! (insert "…"))
 :i "A-l"   (λ! (insert "λ"))

 :n "gb" #'evil-switch-to-windows-last-buffer
 :n "]f" #'dumb-jump-go
 :n "[f" #'dumb-jump-back
 :n "[1" #'+MM|other-file
 :n "]1" #'+MM|other-file

 :leader
 :n "'"   #'+popup/toggle
 :n "au"   #'undo-tree-visualize
 :n "//"   #'helm-projectile-ag

 ;; Fixes for treemacs window navigation
 (:after treemacs-evil
   :n "C-h" #'evil-window-left
   :n "C-l" #'evil-window-right)

 (:desc "Toggle last iBuffer" :n "=" #'+popup/toggle)

 (:desc "search" :prefix "/"
   :desc "Search project" :n  "p" #'helm-projectile-ag)

 (:desc "toggle" :prefix "t"
   :desc "Theme Dark/Light" :n  "t" #'+doom|toggle-theme)

 (:desc "buffer" :prefix "b"
   :desc "Delete File" :n  "D" #'delete-current-buffer-file
   :desc "Delete File" :n  "O" #'doom/kill-matching-buffers
   :desc "Delete File" :n  "X" #'+doom|open-elisp-scratch-buffer)

 (:desc "git" :prefix "g"
   :desc "Checkout"   :n  "b" #'magit-checkout
   :desc "Blame"      :n  "B" #'magit-blame
   :desc "New Branch" :n  "N" #'magit-branch-spinoff)

 (:desc "window" :prefix "w"
   :desc "Split Vertical" :n "|" #'evil-window-vsplit
   :desc "Split Horizontal" :n "_" #'evil-window-split)

 (:desc "project" :prefix "p"
   :desc "services" :n  "s" #'prodigy
   :desc "Workspace Project Files" :n  "P" #'+workspace|find-workspace-project-file)
 (:desc "Yank" :prefix "y"
   :desc "filename"  :n  "f" (λ! (copy-message (file-name-nondirectory buffer-file-name)))
   :desc "base"      :n  "b" (λ! (copy-message (file-name-base (buffer-file-name))))
   :desc "directory" :n  "d" (λ! (copy-message (file-name-directory (buffer-file-name))))
   :desc "path"      :n  "p" (λ! (copy-message (file-name-directory (buffer-file-name))))
   :desc "project"   :n  "r" (λ! (copy-message (s-replace (projectile-project-root) "" (buffer-file-name)))))
 (:desc "workspace" :prefix [tab]
   :desc "Create" :n "c" (λ! (let* ((name (read-string "New workspace name: ")))
                               (+workspace/new name)))
   :desc "Last visited" :n "0" #'+workspace/switch-to-last-visited))

(defvar +Meisterlabs-Web-mode nil)

(def-project-mode! +Meisterlabs-Web-mode
  :when (bound-and-true-p +Meisterlabs-Web-mode-enabled))

(setq projectile-project-search-path '("~/Code/Meisterlabs"))

(after! yasnippet
  :config
  (setq yas-snippet-dirs (append yas-snippet-dirs '("~/Code/Meisterlabs/Snippets"))))

(defun +MM|other-file ()
  "Toggle between component or controller"
  (interactive)
  (setq filename (file-name-nondirectory buffer-file-name))
  (setq path (file-name-directory buffer-file-name))
  (setq target (if (string= filename "component.js") "controller.js" "component.js"))
  (find-file (concat path target)))

(fset '+MM|turn-style-object-into-function
      (lambda (&optional arg)
        "Turns an object into a Style function, needs to be focused on the starting {"
        (interactive "p")
        (kmacro-exec-ring-item (quote ([?y ?s ?a ?B ?b ?i ?S ?t ?y ?l ?e escape ?l ?a ?f ?u ?n ?c ?t ?i ?o ?n ?  S-backspace ?  ?\( ?o ?p ?t ?i ?o ?n ?s ?, ?  ?R ?u ?l ?e ?s escape ?l ?l ?y ?s ?a ?B ?B ?i ?  escape ?l ?a return ?r ?e ?t ?u ?r ?n ?  escape ?l ?j ?> ?i ?\{ ?k ?$ ?% ?a return escape ?k ?a ?\; escape ?= ?= ?j ?b ?l ?%] 0 "%d")) arg)))

(defun +MM|convert-to-new-redux-style ()
  "Converts the current buffer to the new redux style"
  (interactive)
  (shell-command (template "jscodeshift --dry --print --silent --transform ~/Code/Meisterlabs/jscodeshift/redux/v5.8.0/actions-controllers.js <<(buffer-file-name)>>") (current-buffer)))

(fset 'js2r-mm-extract-props
      (lambda (&optional arg)
        "Extract function props to statement"
        (interactive "p")
        (kmacro-exec-ring-item (quote ([?c ?i ?b ?p ?r ?o ?p ?s escape ?o escape ?p ?= ?= ?^ ?i ?c ?o ?n ?s ?t ?  escape ?a escape escape ?A ?  ?= ?  ?p ?r ?o ?p ?s escape ?A ?\; escape ?b ?b ?b ?b ?  ?m ?r ?e ?e ?A ?\C-? ?, escape ?j ?b])) arg)))

(defun dated-string (name)
  (format "%s-name" (format-time-string "%m-%d")))

(defun js2r-mm-taplog ()
  "Insert tap log"
  (interactive)
  (newline-and-indent)
  (yas-lookup-snippet "Tap Console Log" 'js2-mode))


;; (defun +MM|toggle-relative()
;;   (interactive)
;;   )

;; (s-match-strings-all "\\.\\.\\/" "import { foo } from './../../foo")
;; (s-split-up-to "src/" "~/Code/Meisterlabs/mindmeister-web/src/containers/PrivateMaps/ListRow/Map/NonViewable/style.js")

;; (defun +MM|dated-branch ()
;;   "Push the current branch as a dated branch"
;;   (interactive)
;;   (let ((dated-branch (magit-get-current-branch)))

;;     )
;;   (magit-get-current-branch)
;;   )

(defun +dired|kill-dired-buffers ()
  "Kills all dired buffers
Dired creates a buffer for every directory which it visits
Which is fine since you can easily switch between visited buffers
But at some time I want to purge those buffers"
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(map! :when (featurep! :feature evil +everywhere)
      :after dired
      :map dired-mode-map
      :n "Q" #'+dired|kill-dired-buffers)

(after!
  dired
  :config
  ;; Better dired sorting by using the unix ls command instead of the native osx one
  ;; Otherwise the system will come to a crashing halt when using -h flag
  ;; brew install coreutils
  (when (and IS-MAC (locate-file "gls" exec-path))
    (setq dired-listing-switches "-lah")
    (setq insert-directory-program "gls" dired-use-ls-dired t)))

(defun company-select-last ()
  (interactive)
  (company-select-next (- company-candidates-length 1)))

;; Always truncate ElDoc messages to one line. This prevents the echo
;; area from resizing itself unexpectedly when point is on a variable
;; with a multiline docstring.
(setq eldoc-echo-area-use-multiline-p nil)

;; Show ElDoc messages in the echo area immediately, instead of after
;; 1/2 a second.
(setq eldoc-idle-delay 0)

(setq-default magit-save-repository-buffers 'dontask)

(defun shell-command-to-list (cmd)
  "Split output from shell-command to list"
  (split-string (shell-command-to-string cmd) "\n" t))

(defun git-new-files ()
  (shell-command-to-list "git ls-files -om --exclude-standard"))

(defun git-modified-files (branch)
  (shell-command-to-list
   (format "git --no-pager diff --no-renames --name-only --no-merges %s master;" (magit-rev-parse "HEAD"))))

(defun git-get-changed-files (b)
    (delete-dups (append (git-modified-files b) (git-new-files))))

(defun +git|helm-changed-files ()
  (interactive)
  (helm :sources (helm-build-sync-source "Git Changed Files"
                   :candidates (git-get-changed-files "master")
                   :action (helm-make-actions
                            "Find file" (lambda (fs) (find-file (concat (projectile-project-root) fs)))
                            "Search" (lambda (fs) (helm-do-ag nil fs)))
                   :fuzzy-match t)
        :buffer "*helm git changed files"))

(defun magit-revision-show-original-file ()
  "Show the orginal file from a revision buffer
If possible also go to the pointing line"
  (interactive)
  (when magit-buffer-file-name
    (let ((file-name magit-buffer-file-name)
          (line-number (line-number-at-pos))
          (current-line (thing-at-point 'line t)))
      (delete-other-windows)
      (find-file file-name))))
      ;; (when (string= (thing-at-point 'line t) 'current-line)
      ;;   (message "SAME LINE")
      ;;   (goto-line line-number))


(defun +git|commit-search-message-history ()
  "Search and insert commit message from history."
  (interactive)
  (insert (completing-read "History: "
                           ;; Remove unnecessary newlines from beginning and end.
                           (mapcar (lambda (text)
                                     (string-trim text))
                                   (ring-elements log-edit-comment-ring)))))
(defun +git|undo ()
  "Soft reset current git repo to HEAD~1."
  (interactive)
  (magit-reset-soft "HEAD~1"))

(after! magit
  :config
  (setq
   magithub-clone-default-directory "~/Code/Repositories"
   git-commit-summary-max-length 120))
  ;; (bind-key "M-r" #'+git|commit-search-message-history git-commit-mode-map)
  ;; (add-to-list 'savehist-additional-variables log-edit-comment-ring))

(map!
 :leader
 (:desc "Magit" :prefix "g"
   :desc "Changed Files" :n  "F" #'+git|helm-changed-files
   :desc "Fetch" :n  "f" #'magit-fetch-popup
   :desc "Undo" :n  "u" #'+git|undo))

(defun projectile-switch-project-and-do-ag (project)
  "Switch to a project and do a search"
  (let ((projectile-switch-project-action 'helm-projectile-ag))
    (projectile-switch-project-by-name project)))

(after!
  helm-projectile
  :init
  (helm-projectile-define-key helm-projectile-projects-map (kbd "C-/") 'projectile-switch-project-and-do-ag))

;; (defun ar/helm-helm (title candidates on-select-function)
;;   "Helm with TITLE CANDIDATES and ON-SELECT-FUNCTION."
;;   )

;; (defun ar/shell-send-command (command)
;;   "Send COMMAND to shell mode."
;;   ;; (assert (string-equal mode-name "Shell") nil "Not in Shell mode")
;;   (goto-char (point-max))
;;   (comint-kill-input)
;;   (insert command)
;;   (comint-send-input))

;; (defun split-up-to-semi-colon (s)
;;   "Splits a string up to the first semi-colon"
;;   (last (s-split-up-to ";" s 1)))

;; (defun opt (x y)
;;   "Helper function - When x is non-nil use x otherwise use y"
;;   (if x x y))

;; (defun parse-zsh-history (&optional file)
;;   "Read the zsh_history and parse the commands"
;;   (with-temp-buffer
;;     (insert-file-contents (opt file "~/.zsh_history") nil 0 500)
;;     (mapcar 'split-up-to-semi-colon
;;           (delete-dups
;;             (split-string (buffer-string) "\n")))))

;; (defvar +helm|zsh-history
;;   '((name . "Zsh History")
;;     (candidates-process . (lambda)))
;;   )

;; (defun +helm|zsh-history ()
;;   "Narrow down bash history with helm."
;;   (interactive)
;;   (helm :sources +helm|zsh-history
;;         :prompt  "shell command: "
;;         :buffer  "*helm shell history*"))

;; (bind-key "M-r" #'ar/helm-shell-search-history shell-mode-map)

;; ;; Save buffer name
;; ;; Close minibuffer
;; ;; Switch to bufffer

(def-package! indium
  :config
  (setq indium-chrome-executable "/Applications/Google Chrome Canary.app/Contents/MacOS/Google Chrome Canary"))

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

(evil-define-motion evil-motion-insert-newline-below (count)
  "Insert COUNT newlines below"
  :type line
  (save-excursion
    (dotimes (c (or count 1))
      (evil-insert-newline-below))))

(evil-define-motion evil-motion-insert-newline-above (count)
  "Insert COUNT newlines above"
  :type line
  (save-excursion
    (dotimes (c (or count 1))
      (evil-insert-newline-above))))

(defun +flyspell|save-word ()
  "Save the current word to dictionary"
  (interactive)
  (let* ((current-location (point))
         (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

(after! flyspell
  (cond
   ((executable-find "hunspell")

    ;; For the switching, "german" has to be also in this alist
    (add-to-list 'ispell-hunspell-dict-paths-alist (list "german" (expand-file-name "~/Library/Spelling/de_AT.aff")))

    (setq ispell-program-name "hunspell"
          ispell-local-dictionary "en_US"
          ispell-really-hunspell t
          ispell-local-dictionary-alist
                  ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
                  ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
                  '(("english" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "personal,en_US") nil utf-8)
                    ("german"  "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "personal,de_AT") nil utf-8))))))

(defun flyspell-set-language-environment ()
  "Change flyspell language based on the language environment"
  (cond
   ((string= "English" current-language-environment)
    (setq ispell-local-dictionary "english"))
   ((string= "German" current-language-environment)
    (setq ispell-local-dictionary "german"))))

(add-hook 'set-language-environment-hook 'flyspell-set-language-environment)

(setq
 flycheck-javascript-eslint-executable (executable-find "eslint_d")
 flycheck-disabled-checkers '(javascript-jshint javascript))

(after! rjsx-mode
  (add-hook 'js2-mode-hook #'eslintd-fix-mode))
(after! js2-mode
  (add-hook 'js2-mode-hook #'eslintd-fix-mode)
  :config
  (map! :map js2-mode-map
        :localleader
        (:desc "Indium" :prefix "i"
          :desc "Reload" :n  "r" #'indium-reload
          :desc "Start" :n  "s" #'indium-connect)))

(defun remove-js-ext (f)
  "Remove js extension from string"
  (replace-regexp-in-string "\.js$" "" f))

(defun buffer-file-name-relative ()
  "Extranct the filename with extension from path"
  (replace-regexp-in-string (file-name-directory buffer-file-name) "" (buffer-file-name)))

(defun match-const-function-name (line)
  "Matches a line to the word after the declaration"
  (nth 2 (s-match
          "\\(const\\|let\\|class\\)\s\\(.+?\\)\s"
          line)))

(defun const-function-at-point ()
  "Returns the current function name at the current line"
  (match-const-function-name (thing-at-point 'line t)))

(defun js2r-export-default ()
  "Exports the current declaration at the end of the file"
  (interactive)
  (save-excursion
    (let* ((name (const-function-at-point)))
      (goto-char (point-max))
      (insert "\n")
      (insert (template "export default <<name>>;")))))

(defun js2r-extract-const-to-file ()
  "Extracts function to external file"
  (interactive)
  (let* ((name (const-function-at-point))
         (path (concat "./" name ".js")))
    (evil-digit-argument-or-evil-beginning-of-line)
    (js2r-kill)
    (f-write-text "" 'utf-8 path)
    (find-file path)
    (yank)))

(defun js-index-file-names ()
  "Get filenames from current buffers directory"
  (let ((fs (directory-files default-directory nil ".*\\.js")))
    (mapcar 'remove-js-ext
            (remove (buffer-file-name-relative) fs))))

(defun +js|generate-index ()
  "Generate an index import file for files in directory"
  (interactive)
  (erase-buffer)
  (let* ((fs (js-index-file-names)))
    (mapc (lambda (f) (insert "import " f " from './" f "';\n")) fs)
    (insert "\n")
    (insert "export default {\n")
    (mapc (lambda (f) (insert "    " f ",\n")) fs)
    (insert "};")))

(defun js2r-sexp-to-template-string ()
  "Wrap sexp into a template string"
  (interactive)
  (kill-sexp)
  (insert (concat "`${" (substring-no-properties (car kill-ring)) "}`"))
  (pop kill-ring))

(defun +js|load-evil-camel-case-motion ()
  (require 'evil-little-word)
  (define-key evil-normal-state-map (kbd "A-w") 'evil-forward-little-word-begin)
  (define-key evil-normal-state-map (kbd "A-b") 'evil-backward-little-word-begin)
  (define-key evil-operator-state-map (kbd "A-w") 'evil-forward-little-word-begin)
  (define-key evil-operator-state-map (kbd "A-b") 'evil-backward-little-word-begin)
  (define-key evil-visual-state-map (kbd "A-w") 'evil-forward-little-word-begin)
  (define-key evil-visual-state-map (kbd "A-b") 'evil-backward-little-word-begin)
  (define-key evil-visual-state-map (kbd "i A-w") 'evil-inner-little-word))

(after! rjsx-mode
  (+js|load-evil-camel-case-motion))

(after! js2-mode
  (+js|load-evil-camel-case-motion))

(defun +org-web-tools-dwim-at-point ()
  "Pass url to web tools from either:
1. An org link under the cursor
2. An url in the clipboard"
  (interactive)
  (let ((org-url (org-element-property :raw-link (org-element-context)))
        (clipboard-url (current-kill 0)))
    (if org-url
        (message "Reading org url from thing at point")
        (org-web-tools-read-url-as-org org-url)
      (if (string-match url-handler-regexp clipboard-url)
          (message "Reading org url from clipboard")
          (org-web-tools-read-url-as-org clipboard-url)
        (message "No url found")))))

(setq projectile-globally-ignored-file-suffixes '(".org_archive"))

(setq
 org-directory (expand-file-name "~/Dropbox/org")
 org-default-notes-file (concat org-directory "/inbox.org")
 org-shopping-list-file (concat org-directory "/shoppinglist.org")
 org-reading-list (concat org-directory "/reading-list.org")

 org-agenda-start-day "-1d"
 org-agenda-span 5)

(setq
    org-tag-persistent-alist '(
                               ("ACCESABILTY"))
  org-tag-alist '(
                  ("CSS" . ?c)
                  ("DESIGN" . ?d)
                  ("EMACS" . ?e)
                  ("GUIDE" . ?g)
                  ("HASKELL" . ?h)
                  ("JAVASCRIPT" . ?j)
                  ("LEISURE" . ?l)
                  ("MATH" . ?m)
                  ("REASON_ML" . ?r)
                  ("REPOSITORY". ?R)
                  ("WORK" . ?w)))

(defun org-reading-list ()
  (interactive)
  (let ((org-agenda-files (list org-reading-list))
        (org-agenda-prefix-format "%t")
        ;; (org-agenda-sorting-strategy '((timestamp-down)))
        (org-super-agenda-groups
              '((:name "Reading List"
                       :time-grid t
                       :and (:tag ("TEXT")))
                (:name "Watching List" :and (:tag ("VIDEO"))))))
      (org-todo-list)))

(evil-define-key 'motion org-agenda-mode-map
  "vd" 'org-agenda-day-view
  "ds" 'org-agenda-schedule
  "vw" 'org-agenda-week-view
  "vm" 'org-agenda-month-view
  "vy" 'org-agenda-year-view)

(setq org-capture-templates
      (quote (("t" "todo" entry (file+headline org-default-notes-file "INBOX")
               "* [ ] %?\n%U")
              ("c" "Chrome" entry (file+headline org-default-notes-file "INBOX")
               "* %(org-mac-chrome-get-frontmost-url)%?\n%U")

              ("s" "shoppinglist" entry (file org-shopping-list)
               "* Supermarkt\n- [ ] %?")
              ("i" "idea" entry (file+headline org-default-notes-file "INBOX")
               "* %? :IDEA:\n%U")
              ("f" "file" entry (file+headline org-default-notes-file "INBOX")
               "* %?\n%U\n%a")
              ("b" "book" entry (file+headline "~/Dropbox/org/books.org" "Read in the future")
               "*** %?\n%U")
              ("n" "note" entry (file+headline org-default-notes-file "INBOX")
               "* %? :NOTE:\n%U"))))



(defun org-find-file (f)
  "Find file in org directory"
  (find-file (concat org-directory f)))

(defun my-archive-entry ()
  (message "%s" (thing-at-point 'line t)))

(defun +org|org-archive-done-tasks ()
  (interactive)
  (org-map-entries #'my-archive-entry "/[X]" 'tree))

(defun +org|org-archive-done-task ()
  (interactive)
  (org-map-entries (lambda (file) (message file)) "/[X]" 'file))

(defun +org|org-open-home-file ()
  "Open the home org file"
  (interactive)
  (org-find-file "/home.org"))

(defun +org|org-open-reading-list-file ()
  "Open the reading list org file"
  (interactive)
  (org-find-file "/reading-list.org"))

(defun +org|org-open-work-file ()
  "Open the home org file"
  (interactive)
  (org-find-file "/Work/work.org"))

(defun +org|paste-markdown-as-org ()
  "Convert the current clipboard to markdown"
  (interactive)
  (insert (shell-command-to-string "pbpaste | pandoc -f markdown -t org")))

(defun +org|copy-block ()
  "Copies the current block to clipboard"
  (interactive)
  (org-edit-src-code)
  (clipboard-kill-ring-save (point-min) (point-max))
  (org-edit-src-abort))

(defun +org|sort-entries ()
  "Go to header and sort entries"
  (interactive)
  (org-up-element)
  (org-sort)
  (org-shifttab)
  (org-cycle))

(defun +org|narrow-to-subtree ()
  "Narrow to subtree and disable org-indent-mode"
  (interactive)
  (org-narrow-to-subtree)
  (org-indent-mode -1))

(defun +org|narrow-to-block ()
  "Narrow to subtree and disable org-indent-mode"
  (interactive)
  (org-narrow-to-block)
  (org-indent-mode -1))

(defun +org|narrow-to-element ()
  "Narrow to subtree and disable org-indent-mode"
  (interactive)
  (org-narrow-to-element)
  (org-indent-mode -1))

(defun +org|widen ()
  "Widen and enable org-indent-mode"
  (interactive)
  (widen)
  (org-indent-mode t)
  (recenter nil))

(defun +org|paste-chrome-link ()
  "Paste the frontmost chrome link
Fixes wrong paste behaviour where the link would be inserted directly on the character by adding a space
E.g.: (Brackets signal the cursor position)
**[*]
***[]"
  (interactive)
  (unless (looking-at-p "[\s\t\n\r]") (forward-char))
  (insert " ")
  (insert (org-mac-chrome-get-frontmost-url)))

(defun +org|grab-tabs ()
  "Grab all the chrome tabs as an org list to save for later inspection"
  (interactive)
  (let ((tabs
         (do-jxa-script
          (concat
           "Application(\"Chrome\").windows[0].tabs()"
           ".map(tab => `"
           "- [[${tab.url()}][${tab.title()}]]"
           "`)"
           ".join(\"\\n\")"))))
    (insert tabs)))

(defun +org|org-src-block-refmt-reason-ocaml-toggle ()
  "Convert the current src block from ocaml to reason and vice versa"
  (interactive)
  (save-excursion
    (let* ((old-block (org-element-at-point))
           (old-lang (org-element-property :language old-block))
           (new-lang (if (string= old-lang "ocaml") "reason" "ocaml"))
           (formatter (if (string= old-lang "ocaml") 'refmt-region-ocaml-to-reason 'refmt-region-reason-to-ocaml)))
      (org-edit-special)
      (funcall formatter (point-min) (point-max))
      (org-edit-src-exit)
      (let* ((new-block (org-element-at-point))
             (new-block-parsed (org-element-interpret-data (org-element-put-property (org-element-at-point) :language new-lang)))
             (from (org-element-property :begin new-block))
             (to (org-element-property :end new-block)))
        (delete-region from to)
        (insert new-block-parsed)))))

(map! :leader (
               :desc "Notes" :prefix "n"
               :desc "Home" :n  "h" #'+org|org-open-home-file
               :desc "Reading List" :n  "r" #'+org|org-open-reading-list-file
               :desc "Inbox" :n  "i" (λ! (find-file (concat org-directory "/inbox.org")))
               :desc "Work" :n  "w" #'+org|org-open-work-file
               :desc "Agenda" :n  "a" #'org-agenda
               :desc "Store Link" :n  "y" #'org-store-link
               :desc "Save All Org Buffers" :n  "S" #'org-save-all-org-buffers))


;; Journal

(defvar org-journal-dir-default "~/Dropbox/org/journal")
(defvar org-journal-dir-diary "~/Dropbox/org/diary")
(setq org-journal-dir org-journal-dir-default)

(setq org-journal-file-format "%Y-%m-%d")
(setq org-journal-date-prefix "#+TITLE: ")
(setq org-journal-date-format "%A, %B %d %Y")
(setq org-journal-time-prefix "* ")
(setq org-journal-time-format "")


(after! org-agenda
  ;; (setq org-agenda-custom-commands '())
  (add-to-list 'org-agenda-custom-commands
               '("p" "Private" agenda ""
                 ((org-agenda-ndays 5)
                  (org-agenda-span 7)
                  (org-agenda-tag-filter-preset '("-WORK" "-REPEATING"))
                  (tags-todo "-\[X\]")
                  (tags-todo "-DONE")
                  (org-agenda-start-on-weekday 0)
                  (org-agenda-time-grid nil)
                  (org-agenda-start-on-weekday 1)
                  (org-agenda-repeating-timestamp-show-all t))))
  (add-to-list 'org-agenda-custom-commands
               '("w" "Work" tags-todo "+WORK"))
  (add-to-list 'org-agenda-custom-commands
               '("rr" "Reading List" tags-todo "+TEXT")))

(after! org
  (map! :map evil-org-mode-map
        :n "M-k" #'org-move-subtree-up
        :n "M-j" #'org-move-subtree-down

        :localleader
        :desc "Archive Subtree"   :m "a" #'org-archive-subtree
        :desc "Paste Chrome Link" :m "p" #'+org|paste-chrome-link
        :desc "Grab tabs"         :m "P" #'+org|grab-tabs
        :desc "Cut Subtree"       :m "C" #'org-cut-subtree
        :desc "Paste Subtree"     :m "P" #'org-paste-subtree
        :desc "Sort Entries"      :m "S" #'+org|sort-entries

        :desc "Create/Edit Todo"  :nve "o" #'org-todo
        :desc "Schedule"          :nve "s" #'org-schedule
        :desc "Deadline"          :nve "d" #'org-deadline
        :desc "Refile"            :nve "r" #'org-refile
        :desc "Filter"            :nve "f" #'org-match-sparse-tree
        :desc "Tag heading"       :nve "t" #'org-set-tags-command

        (:desc "Insert" :prefix "i"
          :desc "Subheadeing" :m "s" (λ!
                                      (call-interactively 'org-insert-subheading)
                                      (evil-insert-state))
          :desc "Inavtive Timestamp" :m "i" 'org-time-stamp-inactive)
        (:desc "Narrow" :prefix "n"
          :desc "Subtree" :m "s" #'+org|narrow-to-subtree
          :desc "Block"   :m "b" #'+org|narrow-to-block
          :desc "Element" :m "e" #'+org|narrow-to-element
          :desc "widen"   :m "w" #'+org|widen))

  :config

  (setq
   org-todo-keywords '((sequence "[ ](t)" "|" "[X](d)")
                       (sequence "TODO(T)" "DOING(D)" "NEXT(N)" "LATER(L)" "QUESTION(Q)" "|" "DONE(X)" "CANCELLED(C)" "WAITING(W)")))

  ;; Templates
  ;; TODO: Solve this with https://github.com/plexus/a.el
  (add-to-list 'org-structure-template-alist '("es" "#+BEGIN_SRC elisp\n?\n#+END_SRC\n"))
  (add-to-list 'org-structure-template-alist '("E" "#+BEGIN_EXAMPLE\n?\n#+END_EXAMPLE"))
  (add-to-list 'org-structure-template-alist '("j" "#+BEGIN_SRC js\n?\n#+END_SRC\n"))
  (add-to-list 'org-structure-template-alist '("ps" "#+BEGIN_SRC purescript\n?\n#+END_SRC\n"))
  (add-to-list 'org-structure-template-alist '("b" "#+BEGIN_SRC bash\n?\n#+END_SRC\n"))
  (add-to-list 'org-structure-template-alist '("re" "#+BEGIN_SRC reason\n?\n#+END_SRC\n"))
  (add-to-list 'org-structure-template-alist '("oc" "#+BEGIN_SRC ocaml\n?\n#+END_SRC\n"))
  (add-to-list 'org-structure-template-alist '("rb" "#+BEGIN_SRC ruby\n?\n#+END_SRC\n"))
  (add-to-list 'org-structure-template-alist '("md" "#+BEGIN_SRC markdown\n?\n#+END_SRC\n"))

  (defun expand-org-file-names (xs)
    (mapcar (λ (x) (expand-file-name x org-directory)) xs))

  (setq level-1-refile-targets (expand-org-file-names '("reading-list.org"
                                                        "cooking.org"
                                                        ;; "books.org"
                                                        "programming.org"
                                                        "shoppinglist.org")))

  (setq max-level-2-refile-targets (expand-org-file-names '("Emacs.org"
                                                            "art.org"
                                                            "diary"
                                                            "games.org"
                                                            "hardware.org"
                                                            "home.org"
                                                            "inbox.org"
                                                            "mealplan.org"
                                                            "misc.org"
                                                            "movies.org"
                                                            "music.org"
                                                            "osx.org"
                                                            "personal.org"
                                                            "podcasts.org"
                                                            "projects.org"
                                                            "sleep.org"
                                                            "sports.org"
                                                            "travel.org"
                                                            "Work/work.org")))

  (setq org-agenda-files (-concat level-1-refile-targets max-level-2-refile-targets))

  (defun level-1-refile-targets () level-1-refile-targets)

  (defun max-level-2-refile-targets () max-level-2-refile-targets)

  (setq
   org-agenda-start-on-weekday 1
   org-image-actual-width 600
   org-refile-targets (quote ((nil :maxlevel . 5)
                              (max-level-2-refile-targets :maxlevel . 2)
                              (level-1-refile-targets :level . 1)))
   org-agenda-refile org-agenda-files
   org-default-notes-file (concat org-directory "/inbox.org")))

(defun rtop ()
  "Launch reason version of utop"
  (interactive)
  (cl-letf ((utop-command "rtop -emacs"))
    (utop)))

(def-package! reason-mode
  :mode "\\.rei?$"
  :commands (reason-mode)
  :config
  ;; Merlin eldoc is very slow with marking the whole type region
  ;; Just trigger it via C-c C-t
  (setq merlin-eldoc-doc nil)
  (let* (
         (refmt-bin (executable-find "refmt"))
         (merlin-bin (executable-find "ocamlmerlin"))
         (merlin-base-dir (when merlin-bin
                            (replace-regexp-in-string "bin/ocamlmerlin$" "" merlin-bin))))
    ;; Add npm merlin.el to the emacs load path and tell emacs where to find ocamlmerlin
    (when merlin-bin
      (add-to-list 'load-path (concat merlin-base-dir "share/emacs/site-lisp/"))
      (setq merlin-command merlin-bin))

    (when refmt-bin
      (setq refmt-command refmt-bin))
    (require 'merlin)
    (add-hook! reason-mode
        (add-hook 'before-save-hook #'refmt-before-save nil t)
        (merlin-mode))
    (setq-hook! reason-mode
        indent-region-function #'apply-refmt)
    (set-electric! 'some-mode :chars '(?|))
    (set-lookup-handlers! 'reason-mode
                          :definition #'merlin-locate
                          :references #'merlin-occurrences
                          :documentation #'merlin-document)
    (set-company-backend! 'reason-mode 'merlin-company-backend)))

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
