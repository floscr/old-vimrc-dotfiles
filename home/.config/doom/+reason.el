;;;  -*- lexical-binding: t; -*-

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
