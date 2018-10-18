;;; ~/.homesick/repos/Dotfiles/home/.config/doom/+ui.el -*- lexical-binding: t; -*-

(remove-hook 'doom-init-ui-hook #'blink-cursor-mode)
(blink-cursor-mode -1)

(cond
 ((string= system-name "Florians-iMac.local")
  (setq-default line-spacing 0.3))
 ((string= system-name "Florians-MacBook-Air.local")
  ;; Fix for small helm ui on small display
  (set-popup-rule! "^\\*helm" :vslot -100 :size 0.32 :ttl nil))
 (t (setq-default line-spacing 0.15)))

(setq
 scroll-conservatively 10
 scroll-margin 10)

;; Remove Scrolloff for terminal
(add-hook 'term-mode-hook (λ! (setq-local scroll-margin 0)))

(defconst light-theme 'doom-one)
(defconst dark-theme  'doom-one-light)

(defun +doom|toggle-theme ()
  "Toggle between light and dark themes."
  (interactive)
  (cond ((eq doom-theme dark-theme)
         (message "Toggling to light-theme: %s" light-theme)
         (setq doom-theme light-theme)
         (doom/reload-theme))
        ((eq doom-theme light-theme)
         (message "Toggling to dark-theme: %s" dark-theme)
         (setq doom-theme dark-theme)
         (doom/reload-theme))
        (t (message "Toggling theme is not possible. Theme is not currently light-theme (%s) or dark-theme (%s)." light-theme dark-theme))))

(defun +ui|increase-line-spacing ()
  (interactive)
  (setq-default line-spacing (+ line-spacing 0.2)))

(defun +ui|decrease-line-spacing ()
  (interactive)
  (setq-default line-spacing (- line-spacing 0.2)))

