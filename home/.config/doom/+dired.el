;;; ~/.homesick/repos/Dotfiles/home/.config/doom/+dired.el -*- lexical-binding: t; -*-

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
