;;; ~/.homesick/repos/Dotfiles/home/.config/doom/+ui.el -*- lexical-binding: t; -*-

(remove-hook 'doom-init-ui-hook #'blink-cursor-mode)
(blink-cursor-mode -1)

(cond
 ((string= system-name "Florians-iMac.local")
  (setq-default line-spacing 0.3))
 ((string= system-name "Florians-MacBook-Air.local")
  (setq-default line-spacing 0.4)
  (setq initial-frame-alist
        (append (list '(left . 272)
                      '(width . 165)
                      '(fullscreen . fullheight))
                initial-frame-alist))
  (after! helm-mode
    :config
    ;; Fix for small helm ui on small display
    (set-popup-rule! "^\\*helm" :vslot -100 :size 0.32 :ttl nil)))
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

(defvar default-line-spacing 0.2)

(defun set-line-spacing (&optional spacing)
  "Set the line spacing
When no line spacing is given is the default-line-spacing"
  (if line-spacing
      (setq-default line-spacing (+ (or spacing default-line-spacing) line-spacing))
    (setq-default line-spacing (+ 0 default-line-spacing))))

(defun +ui|reset-line-spacing ()
  (interactive)
  (setq-default line-spacing nil))

(defun +ui|increase-line-spacing ()
  (interactive)
  (set-line-spacing))

(defun +ui|decrease-line-spacing ()
  (interactive)
  (set-line-spacing (- default-line-spacing)))
