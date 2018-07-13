;;; ~/.homesick/repos/Dotfiles/home/.config/doom/spotify.el -*- lexical-binding: t; -*-

(defun git-changed-files ()
  (projectile-with-default-dir (projectile-project-root)
    (substring (shell-command-to-string "git --no-pager diff --no-renames --name-only --no-merges $(git rev-parse --abbrev-ref HEAD) master; git ls-files -om --exclude-standard") 0 -1))
  )

(git-changed-files)

(helm :sources (helm-build-sync-source "test"
                 :candidates 'git-changed-files
                 :fuzzy-match t)
      :buffer "*helm test*")


