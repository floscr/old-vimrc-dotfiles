;;; ~/.homesick/repos/Dotfiles/home/.config/doom/+MM.el -*- lexical-binding: t; -*-

(after! yasnippet
  :config
  (setq yas-snippet-dirs (append yas-snippet-dirs '("~/Code/Meisterlabs/Snippets")))
  )

(defun +MM|other-file ()
  "Toggle between component or controller"
  (interactive)
  (setq filename (file-name-nondirectory buffer-file-name))
  (setq path (file-name-directory buffer-file-name))
  (setq target (if (string= filename "component.js") "controller.js" "component.js"))
  (find-file (concat path target))
  )

(fset '+MM|turn-style-object-into-function
   (lambda (&optional arg)
     "Turns an object into a Style function, needs to be focused on the starting {"
     (interactive "p")
     (kmacro-exec-ring-item (quote ([?y ?s ?a ?B ?b ?i ?S ?t ?y ?l ?e escape ?l ?a ?f ?u ?n ?c ?t ?i ?o ?n ?  S-backspace ?  ?\( ?o ?p ?t ?i ?o ?n ?s ?, ?  ?R ?u ?l ?e ?s escape ?l ?l ?y ?s ?a ?B ?B ?i ?  escape ?l ?a return ?r ?e ?t ?u ?r ?n ?  escape ?l ?j ?> ?i ?\{ ?k ?$ ?% ?a return escape ?k ?a ?\; escape ?= ?= ?j ?b ?l ?%] 0 "%d")) arg)
     ))
