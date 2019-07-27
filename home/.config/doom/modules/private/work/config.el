;; -*- lexical-binding: t -*-

(defvar +MM-Web-mode nil)

(def-project-mode! +MM-Web-mode
  :when (bound-and-true-p +MM-Web-mode-enabled)
  :on-enter
  (map!
   :n "[1" #'+MM|other-file
   :n "]1" #'+MM|other-file))

(defun +MM|alternate-file ()
  "Find the alternate file for the current buffer."
  (let
      ((file-path (file-name-nondirectory buffer-file-name))
       (dir-path (file-name-directory buffer-file-name))
       (alternate-file (if (string= file-path "component.js") "controller.js" "component.js")))
    (find-file (f-join dir-path alternate-file))))

(fset '+MM|turn-style-object-into-function
      (lambda (&optional arg)
        "Convert a style object into a Style function, needs to be focused on the starting {"
        (interactive "p")
        (kmacro-exec-ring-item (quote ([?y ?s ?a ?B ?b ?i ?S ?t ?y ?l ?e escape ?l ?a ?f ?u ?n ?c ?t ?i ?o ?n ?  S-backspace ?  ?\( ?o ?p ?t ?i ?o ?n ?s ?, ?  ?R ?u ?l ?e ?s escape ?l ?l ?y ?s ?a ?B ?B ?i ?  escape ?l ?a return ?r ?e ?t ?u ?r ?n ?  escape ?l ?j ?> ?i ?\{ ?k ?$ ?% ?a return escape ?k ?a ?\; escape ?= ?= ?j ?b ?l ?%] 0 "%d")) arg)))

(defun +MM|convert-to-new-redux-style ()
  "Converts the current buffer to the new redux style."
  (interactive)
  (shell-command (template "jscodeshift --dry --print --silent --transform ~/Code/Meisterlabs/jscodeshift/redux/v5.8.0/actions-controllers.js <<(buffer-file-name)>>") (current-buffer)))

(defun +MM|create-action ()
  "Create an action file from a controller file"
  (interactive)
  (let* ((action-name (read-string "Action Name: "))
         (root-dir (file-name-directory (buffer-file-name)))
         (actions-dir (concat root-dir "actions/"))
         (actions-index-file (concat actions-dir "index.js"))
         (action-file (concat actions-dir (concat action-name ".js"))))
    (unless (file-directory-p actions-dir) (mkdir actions-dir))
    (copy-file
     (concat (projectile-project-root) "snippets/Core/Action.js")
     action-file)
    (with-temp-file actions-index-file
      (+js|generate-index actions-dir))
    (find-file action-file)))

(defun +MM|canvas-create-action ()
  "Create an action file from a controller file"
  (interactive)
  (let* ((action-name (read-string "Action Name: "))
         (actions-dir (f-join (projectile-project-root) "src/containers/MapEditor/actions"))
         (actions-index-file (f-join actions-dir "index.js"))
         (action-file (f-join actions-dir (concat action-name ".js"))))
    (copy-file
     (f-join (projectile-project-root) "snippets/Core/Action.js")
     action-file)
    (with-temp-file actions-index-file
      (+js|generate-index actions-dir))
    (find-file action-file)))

(defun +MM|canvas-files ()
  "Project files but only for canvas"
  (interactive)
  (ivy-read "Find File: "
            (--filter (s-contains? "MapEditor" it) (projectile-current-project-files))
            :action #'projectile-find-file))

(setq +MM-comment-headers '("EXTERNALS" "LOCALS" "HELPERS" "MAIN"))

(defun +MM:get-remaining-headers-list (header)
  "Returns a reversed list of headers to search through"
  (->> +MM-comment-headers
       (-split-on header)
       (-first-item)))

(defun +MM:goto-or-add-header (header)
  "Goes to a comment header or if it doesnt exist creates one"
  (goto-char (point-min))
  (if (search-forward header nil t)
      (progn
        (search-forward-regexp "^/\\*\\*" nil t)
        (previous-line 2))
    ;; Create header when none was found
    (progn
      (let* ((headers (reverse (+MM:get-remaining-headers-list header))))
        (--first (search-forward it nil t) headers))
      (search-forward-regexp "^/\\*\\*" nil t)
      (evil-insert-newline-above)
      (insert (template  "/** <<header>> **/\n\n\n\n"))
      (previous-line 1))))

(defun +MM:add-import-to-file (file)
  (goto-char (point-min))
  (let* ((is-local (s-contains? "./" file))
         (comment-header-title (if is-local "LOCALS" "EXTERNALS")))
    (+MM:goto-or-add-header comment-header-title)
    (evil-insert-newline-below)
    (previous-line 1)
    (+js/import-file file)))

(defun +MM|import-file ()
  (interactive)
  (+js|ivy-import-file '+MM:add-import-to-file))

(defun +MM|new-worktree ()
  "New worktree with the devpanel files and an npm install"
  (interactive)
  (let* ((origin-path (projectile-project-root))
         (path (call-interactively 'magit-worktree-branch-project-worktree))
         (local-devutil-file (f-join origin-path "src/apps/Main/DevPlugin.local.js"))
         (worktree-devutil-file (f-join path "src/apps/Main/DevPlugin.local.js")))
    (when (file-exists-p local-devutil-file)
      (copy-file local-devutil-file worktree-devutil-file))
    (npm-mode)
    (npm-mode-npm-ci)))

(after! magit
  (transient-append-suffix 'magit-worktree "y" '("m" "+MM|Worktree" +MM|new-worktree)))
