;;; ~/.homesick/repos/Dotfiles/home/.config/doom/+helm.el -*- lexical-binding: t; -*-

;; https://github.com/hitswint/.emacs.d/blob/794cf1db4a1c2d4f5866c8507e4d39386fb4847d/lisp/setup_projectile.el#L34
; (helm-projectile-define-key helm-projectile-projects-map (kbd "C-1") '(lambda (project) (helm-projectile-ag "--hidden")))

(defun projectile-switch-project-and-do-ag (project)
  "Switch to a project and do a search"
  (let ((projectile-switch-project-action 'helm-projectile-ag))
    (projectile-switch-project-by-name project)))

(after!
  helm-projectile
  :config
  (helm-projectile-define-key helm-projectile-projects-map (kbd "C-/") 'projectile-switch-project-and-do-ag))

(after! helm
  (add-hook! 'helm-find-files-after-init-hook
    (map! :map helm-find-files-map
          "<DEL>" #'helm-find-files-up-one-level))
