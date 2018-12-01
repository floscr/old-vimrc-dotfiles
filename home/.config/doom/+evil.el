;;; ~/.homesick/repos/Dotfiles/home/.config/doom/+evil.el -*- lexical-binding: t; -*-

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
