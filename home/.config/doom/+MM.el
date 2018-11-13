;;; ~/.homesick/repos/Dotfiles/home/.config/doom/+MM.el -*- lexical-binding: t; -*-

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
