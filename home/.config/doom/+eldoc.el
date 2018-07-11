;;; ~/.homesick/repos/Dotfiles/home/.config/doom/+eldoc.el -*- lexical-binding: t; -*-

;; Always truncate ElDoc messages to one line. This prevents the echo
;; area from resizing itself unexpectedly when point is on a variable
;; with a multiline docstring.
(setq eldoc-echo-area-use-multiline-p nil)

;; Show ElDoc messages in the echo area immediately, instead of after
;; 1/2 a second.
(setq eldoc-idle-delay 0)
