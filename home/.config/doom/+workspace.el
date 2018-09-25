;;; ~/.homesick/repos/Dotfiles/home/.config/doom/+workspace.el -*- lexical-binding: t; -*-

(defun +workspace/switch-to-last-visited ()
  "Switch to the last visited workspace."
  (interactive)
  (+workspace/switch-to +workspace--last))
