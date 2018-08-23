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

(after!
  dired
  :config
  ;; Better dired sorting by using the unix ls command instead of the native osx one
  ;; Otherwise the system will come to a crashing halt when using -h flag
  ;; brew install coreutils
  (when (and IS-MAC (locate-file "gls" exec-path))
    (setq dired-listing-switches "-lah")
    (setq insert-directory-program "gls" dired-use-ls-dired t)))
