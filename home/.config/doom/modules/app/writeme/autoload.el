;;; app/write/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar +write-mode-map (make-sparse-keymap)
  "TODO")

;;;###autoload
(define-minor-mode +write-mode
  "Turns Emacs into a more comfortable writing environment and word processor."
  :init-value nil
  :keymap +write-mode-map
  (setq-local visual-fill-column-center-text t)
  (when +write-text-scale
    (text-scale-set (if +write-mode 6 0)))
  (setq-local line-spacing 0.8))

;;;###autoload
(defun +write|init-org-mode ()
  "Initializes `org-mode' specific settings for `+write-mode'."
  (when (eq major-mode 'org-mode)
    (+org-pretty-mode (if +write-mode +1 -1))))

;;;###autoload
(defun +write|init-line-numbers ()
  (display-line-numbers-mode -1))

;;;###autoload
(defun +write|init-mixed-pitch ()
  (copy-face 'variable-pitch 'write-variable-pitch)
  ;; TODO Hacky workaround to set the variable pitch font just for the current buffer
  (set-face-attribute 'variable-pitch nil :font "Georgia")
  (mixed-pitch-mode (if +write-mode +1 -1))
  (set-face-attribute 'variable-pitch nil :font "Helvetica"))

;;;###autoload
(defun +write|init-visual-fill-column ()
  (visual-fill-column-mode (if +write-mode +1 -1)))

;;;###autoload
(add-hook! '+write-mode-hook
  #'(visual-line-mode
     +write|init-mixed-pitch
     +write|init-visual-fill-column
     +write|init-line-numbers
     +write|init-org-mode))
