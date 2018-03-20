;; Fix the colors for osx
(setq ns-use-srgb-colorspace nil)
;; Store undos after closing emacs
;; enabling to undo after closing a file
(global-undo-tree-mode)
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
